#include <alloca.h>
#include <stddef.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include "nuc.h"

nuc_val rt_exec(nuc_val path_val, nuc_val arg_list)
{
	char *path = rt_nuc_str_to_c_str(path_val);

	char **argv;
	char *no_args[] = { path };
	if (arg_list == NIL) {
		argv = no_args;
	} else {
		CHECK(arg_list, CONS_LOWTAG);

		Cons *cons = (Cons *)REMOVE_LOWTAG(arg_list);
		size_t args_len = rt_list_length(cons) + 1;

		argv = alloca((args_len + 1) * sizeof *argv);
		argv[0] = path;
		argv[1] = rt_nuc_str_to_c_str(cons->car);
		size_t i = 2;
		Cons *tmp = cons;
		for (;;) {
			nuc_val next = tmp->cdr;
			if (next == NIL)
				break;

			CHECK(next, CONS_LOWTAG);
			tmp = (Cons *)REMOVE_LOWTAG(next);
			argv[i++] = rt_nuc_str_to_c_str(tmp->car);
		}

		argv[args_len] = NULL;
	}

	pid_t pid = fork();
	if (pid == 0)
		execvp(path, argv);
	
	int status;
	waitpid(pid, &status, 0);
	return INT_TO_NUC_VAL(status);
}
