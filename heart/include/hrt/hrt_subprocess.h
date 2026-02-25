#ifndef HRT_SUBPROCESS_H

#define HRT_SUBPROCESS_H

#include <fcntl.h>

#include <hrt/hrt_server.h>

enum hrt_exec_result {
    HRT_EXEC_SUCCESS,
    HRT_EXEC_PIPE_ERROR,
    HRT_EXEC_FORK_ERROR,
    HRT_EXEC_EXEC_ERROR,
    HRT_EXEC_INIT_ERROR,
};

enum hrt_collect_result {
    HRT_COLLECT_RESULT_SUCESS,
    HRT_COLLECT_RESULT_ERROR,
};

/**
 * Callback used when a subprocess exits.
 * @param output the stdout of the program that was executed
 * @param n_output the size of the output string. If an error occured,
 * the output string may not be null-terminated, so use this instead.
 *
 * This function is responsible for freeing the OUTPUT string.
 **/
typedef void (*hrt_exec_callback_fn)(char *output, size_t n_output,
                                     enum hrt_collect_result result, pid_t pid,
                                     void *data);

/**
 * @param argv The args used to start a subprocess as defined by execvp.
 * @param pid The PID of the started process. Is unchanged if execution fails.
 **/
enum hrt_exec_result hrt_output_from_subprocess(struct hrt_server *server,
                                            char *const argv[],
                                            hrt_exec_callback_fn callback_fn,
                                            void *data, pid_t *pid);

#endif
