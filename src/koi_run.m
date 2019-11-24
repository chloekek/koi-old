% This module implements the running of deployments.
:- module koi_run.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- interface.

:- import_module array.
:- import_module io.

:- pred fork_exec(string, array(string), int, io, io).
:- mode fork_exec(in, in, out, di, uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- pragma foreign_proc(
    "C",
    fork_exec(Program::in, Arguments::in, Status::out, _IO1::di, _IO2::uo),
    [will_not_call_mercury, promise_pure],
    "
        pid_t pid = fork();

        if (pid == -1)
        {
            Status = -1;
        }

        else if (pid == 0)
        {
            size_t ArgCount = Arguments->size;
            char** Args     = (char**)Arguments->elements;

            char** ExecArgs = malloc(sizeof(char*) * ArgCount + 1);
            memcpy(ExecArgs, Args, sizeof(char*) * ArgCount);
            ExecArgs[ArgCount] = NULL;

            execvp(Program, ExecArgs);
            exit(1);
        }

        else
        {
            int wstatus = -1;
            while (waitpid(pid, &wstatus, 0) == -1 && errno == EINTR)
                ;
            Status = wstatus;
        }
    "
).
