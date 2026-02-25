#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include <hrt/hrt_server.h>
#include <wayland-server-core.h>

#include <hrt/hrt_subprocess.h>

enum read_result {
    DONE,
    MORE_INPUT,
    ALLOC_ERROR,
};

struct result_state {
    char *result;
    size_t result_size;
    size_t capacity;
};

// The buffer and alloc sizes were chosen arbitrarily. The idea is to be
// able to support and arbitrary input size while not allocating a bunch
// that we don't need.
constexpr size_t alloc_size = 4096;

static bool init_buff_result(struct result_state *result) {
    result->result = calloc(alloc_size, sizeof(char));
    if (!result) {
        perror("Buffer calloc");
        return false;
    }
    result->result_size = 0;
    result->capacity    = alloc_size;
    return true;
};

static bool add_null_char(struct result_state *state) {
    if (state->capacity < state->result_size + 1) {
        state->result = realloc(state->result, state->result_size + 1);
        if (!state->result) {
            return false;
        }
    }
    state->result[state->result_size] = '\0';
    state->result_size += 1;
    return true;
}

static enum read_result read_from_file(FILE *input,
                                       struct result_state *state) {
    // We could probably get rid of this buffer by loading the results
    // directly into the results array?
    char buffer[256];
    size_t read_size;
    static_assert(sizeof(buffer) <= alloc_size);
    do {
        read_size = (size_t)fread(buffer, sizeof(char), sizeof(buffer), input);
        const size_t new_size = state->result_size + read_size;
        if (new_size > state->capacity) {
            const size_t new_alloc =
                (state->capacity + alloc_size) * sizeof(char);
            state->result = realloc(state->result, new_alloc);
            perror("Reallocateting buffer");
            if (!state->result) {
                return ALLOC_ERROR;
            }
            state->capacity = new_alloc;
        }
        memcpy(state->result + state->result_size, buffer, read_size);
        state->result_size = new_size;
        // We go until we read less that what we expect, which means
        // we hit the end of what is available:
    } while (read_size == sizeof(buffer));

    if (feof(input)) {
        puts("Read succcesfully");
        add_null_char(state);
        return DONE;
    } else {
        puts("Failure in last read");
        return MORE_INPUT;
    }
}

struct subprocess_data {
    FILE *file;
    pid_t pid;
    struct wl_event_source *event_source;
    struct result_state result;
    hrt_exec_callback_fn fn;
    void *data;
};

static void handle_finish(struct subprocess_data *data,
                          enum hrt_collect_result result) {
    wl_event_source_remove(data->event_source);
    fclose(data->file);

    // Call free() before the callback to ensure things are cleaned up
    // even in case of an error. This should make lisp debugging a bit easier:
    char *output            = data->result.result;
    size_t n_output         = data->result.result_size;
    hrt_exec_callback_fn fn = data->fn;
    pid_t pid               = data->pid;
    void *callback_data     = data->data;
    free(data);

    fn(output, n_output, result, pid, callback_data);
}

static int handle_output(int fd, uint32_t mask, void *data) {
    assert(!(mask & WL_EVENT_ERROR));
    struct subprocess_data *info = data;

    // We get close events at the same time as others, so make sure to read
    // first and exit if we detect a close event:
    if (mask & WL_EVENT_READABLE) {
        enum read_result read_result =
            read_from_file(info->file, &info->result);
        if (read_result == DONE) {
            handle_finish(info, HRT_COLLECT_RESULT_SUCESS);
            return 0;
        } else if (read_result == ALLOC_ERROR) {
            handle_finish(info, HRT_COLLECT_RESULT_ERROR);
            return 0;
        }
    }

    if (mask & WL_EVENT_ERROR) {
        handle_finish(info, HRT_COLLECT_RESULT_ERROR);
        return 0;
    }
    if (mask & WL_EVENT_HANGUP) {
        handle_finish(info, HRT_COLLECT_RESULT_ERROR);
        return 0;
    }

    assert(false); // should not reach this line
    return 0;
}

#define READ_END  0
#define WRITE_END 1

enum hrt_exec_result
hrt_output_from_subprocess(struct hrt_server *server, char *const argv[],
                           hrt_exec_callback_fn callback_fn, void *data,
                           pid_t *pid) {
    int pipefd[2];

    if (pipe(pipefd) == -1) {
        wlr_log(WLR_ERROR, "Failed to allocate pipe");
        return HRT_EXEC_PIPE_ERROR;
    }

    // 2. Fork a child process
    pid_t cpid = fork();
    if (cpid == -1) {
        wlr_log(WLR_ERROR, "Failed to fork process");
        return HRT_EXEC_FORK_ERROR;
    }

    if (cpid == 0) { // Child process
        // Hook up the pipe to out stdout:
        dup2(pipefd[WRITE_END], STDOUT_FILENO);
        close(pipefd[READ_END]);
        close(pipefd[WRITE_END]);

        int result = execvp(argv[0], argv);
        wlr_log(WLR_ERROR, "Failed to start process %s with error %d", argv[0],
                result);
        return HRT_EXEC_EXEC_ERROR;
    } else { // Parent process
        // We don't use the write end, so close it immediately:
        close(pipefd[WRITE_END]);
        FILE *input = fdopen(pipefd[READ_END], "r");
        struct subprocess_data *data =
            calloc(1, sizeof(struct subprocess_data));
        data->file = input;
        data->data = data;
        data->fn   = callback_fn;
        data->pid  = cpid;
        *pid       = cpid;

        if (!init_buff_result(&data->result)) {
            fclose(input);
            free(data);
            return HRT_EXEC_INIT_ERROR;
        }

        struct wl_event_loop *event_loop =
            wl_display_get_event_loop(server->wl_display);
        uint32_t mask = WL_EVENT_READABLE;
        // Maybe we could reuse pipefd instead of fileno? fileno seems safer:
        data->event_source = wl_event_loop_add_fd(event_loop, fileno(input),
                                                  mask, &handle_output, data);
        return HRT_EXEC_SUCCESS;
    }
}
