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

Due to the amount of guarantees that deployments need to provide being small,
writing one is very easy: any executable will do.
An executable consumes the state of the system, giving new state,
simply by mutating the state of the system in place.

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
