Deployments
===========

Let’s start by looking at deployments at a high level.
We will later look at how deployments are described
in the Koi programming language,
by using what we have learned here.

A **deployment** is a function
that consumes the state of a system
and produces the next state of the system.
After applying a deployment,
the previous state is no longer accessible.
A deployment can thus be seen as a function
of the following type:

.. math::

   \text{Deployment} = \text{State} \multimap \text{State}

Creating new deployments
------------------------

Physically, a deployment is a value that pairs
an archive of files with a command.
**Running** a deployment executes the command
in a temporary directory in which this archive is expanded.
The command is expected to inspect the current state of the system
and alter it in such a way that the system is in the new state.
Typical contents of the archive
are static files that the command needs,
such as configuration files or computer programs.
After the deployment is run,
the temporary directory is removed.

The primitive function :math:`\text{deployment}`
creates a deployment from an archive of files and a command:

.. math::

   \text{deployment}(-, -)
      : \text{File}^\star \times \text{String}^+ \to \text{Deployment}

In fact, assuming the usual behavior of the program *true(1)*,
the following equation should hold:

.. math::

   \text{deployment}(\langle\rangle, \langle\texttt{"/bin/true"}\rangle) = 1

Combining deployments
---------------------

Two deployments can be combined
to form a new deployment by **composition**.
The infix operator :math:`\circ` denotes composition of deployments:

.. math::

   - \circ - : \text{Deployment} \times \text{Deployment} \to \text{Deployment}

A deployment :math:`a \circ b`
first applies the deployment :math:`b`,
then applies the deployment :math:`a`.
Because the composition operator takes *and* returns a deployment,
there is no limit to how many deployments can be composed.

Composition of deployments is associative;
it doesn’t matter where you put the parentheses
in a chain of deployments:

.. math::

   \forall\,a\,b\,c. a \circ (b \circ c) = (a \circ b) \circ c

The identity deployment
-----------------------

The **identity deployment**, denoted :math:`1`,
is a special deployment that
does not alter the state of the system.
The identity deployment is useful in situations
where you need a deployment but you don’t actually
want to apply any changes:

.. math::

   \forall\,a. a \circ 1 = 1 \circ a = a

Deploying to servers
--------------------

Running a deployment normally runs it on
the machine that Koi is invoked on.
The primitive combinator :math:`\text{remote}`
transforms a given deployment
such that it runs on a different host over SSH:

.. math::

   \text{remote}(-, -)
      : \text{Host} \times \text{Deployment} \to \text{Deployment}

For instance, if a deployment :math:`a`
contains two files and a command,
:math:`\text{remote}(h, a)` returns a new deployment
that uploads the two files to host :math:`h`
and subsequently runs the command over SSH.
