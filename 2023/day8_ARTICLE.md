# The underlying issue with Advent of Code 2023 day 8, part 2

The Advent of Code 2023 [day 8](https://adventofcode.com/2023/day/8) input has very special properties that are not mentioned in the problem description, yet they greatly simplify solving part 2. I first tried solving the more general problem, but I gave up when I had to write the code for practical and aesthetic reasons. Instead, I solved the specific problem, which was far more trivial.

After my solution descriptions and discussing why I gave up on the general problem, I give an extended discussion about how all the nasty business I endured result from issues of the problem itself (and not because of me).

## The specific solution

After struggling with the general solution, I ended up writing a solution for the problem using the unmentioned special properties of the input, and moving on with my life. However, rather than accepting a complete failure, I document here my algorithm to count this endeavor as a partial success.

The special properties are described in the rest of this paragraph &mdash; containing spoilers for the problem, naturally. The connected components of the given graph (of the specified state transition machine) are double-thick circles of circumference divisible by the length of the instruction list. The double-thickness merely records whether the last instruction was left or right. For each circle, there is a starting node that points to the nodes directly after the ending node. This means that we merely need to take the least common multiple of the circumferences of the circles, measured by how many steps it takes to reach an ending node from a starting node. 

It is hard to emphasize how much these properties trivialize the problem.

## The general solution

The `day8_alt*.scm` files are partial attempts to solve the more general problem in a somewhat efficient manner, which is as follows. **Feel free to skim this section.**

### Description

Let the state space $S$ consist of pairs of location labels and integers modulo $n$, where $n$ is the length of the instruction list. That is, $S=\text{Labels}\times\Z/n\Z$. Then, taking a step can be thought of as an endofunction (self-mapping) on $S$. This can be thought of as a discrete dynamical system, where iteration of the endofunction evolves the system over time. Since $S$ is finite, each point is either periodic (recurs under iteration) or preperiodic (does not recur, but evolves to a periodic point).

For each starting point $x$ specified in the problem (labels ending in `A` beginning at instruction $0\bmod n$), we follow the trajectory and find possible ending points (labels ending in `Z`), remembering their distances from the starting point. By marking points as visited, we terminate the search after revisiting a point, separating the data into preperiodic and periodic parts $P_x$ and $Q_x$ and noting the period $n_x$. (If we assume an ending point will always be in the periodic part, we need only mark ending points as visited.) Write $a_x = (P_x, Q_x, n_x) \in \mathcal{P}(\Z)\times\mathcal{P}(\Z)\times\Z$ for this data, where $\mathcal{P}(X)$ is the set of subsets (power set) of $X$. Write $\pi_P, \pi_Q, \pi_n$ for the projection functions that retrieve the first, second, or third datum respectively of a triple from $\mathcal{P}(\Z)\times\mathcal{P}(\Z)\times\Z$. Call these triples partial solutions &mdash; notice that $a_x$ consists of solutions satisfying constraints given by the trajectory of $x$.

Finally, we unify our data. If there are no starting points, we clearly need $0$ steps for all ghosts to be on ending points. Otherwise, we merge partial solutions two at a time, i.e. using `reduce` on the above data. Given partial solutions $x,y$, we compute a new partial solution $z = (P', Q', n')$ as follows, which is understood be partial solution data constrained by both sets of constraints on $x$ and $y$.

First, we define some helpful notation and data. We use the common notation $L + u$ to mean $\{l+u ~|~ l\in L\}$. For any partial solution $s$ with nonempty $\pi_Q s$, let $m_s = \min \pi_Q s$ and $R_s = \pi_Q t - m_s$, where $R_s$ may be considered a subset of the integers modulo $n_s$.

Let $P' = (\pi_P x \cap \pi_P y)\cup \kappa(x,y) \cup \kappa(y,x)$, where $\kappa(s,t)$ is the empty set when $\pi_Q t$ is empty, otherwise $\kappa(s,t) = \{p\in \pi_P s~|~p\ge m_t \text{ and } (p-m_t)\bmod \pi_n t\in R_t\}$. Note that $\kappa(s,t)$ computes which preperiodic ending points in the trajectory of $s$ occur at the same time as periodic ending points in the trajectory of $t$. (Of course, the predicate can be simplified and constants precomputed.) 

We compute $Q'$ and $n'$. If either $\pi_Q x$ or $\pi_Q y$ is empty, $Q'$ is empty. Otherwise, without loss of generality (i.e. swapping names if needed), require $m_x \le m_y$. Let $R = R_x - m_x$ and $T = R_y - m_y$, using arithmetic modulo $n_y$. Use the [extended Euclidean algorithm](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm) to compute the Bézout coefficients $a,b$ and the greatest common divisor $g = \gcd(\pi_n x,\pi_n y)$ in [Bézout's identity](https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity) $a\pi_n x + b\pi_n y = g$, and let $n' = (\pi_n x)(\pi_n y)/g$. Finally, let $Q' = \{(ta+rb)/g \mod n' ~|~ (r,t)\in U \text{ and } r \equiv t \mod g\} + m_y$, where the final addition is performed in the integers. (The extended Euclidean algorithm can compute extra data to simplify this calculation.) The [formula on the left side](https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Generalization_to_non-coprime_moduli) gives solutions modulo $n'$ that lift compatible solutions modulo $\pi_n x$ and $\pi_n y$, and the predicate on the right side ensures said compatibility.

Once we reduce our list of partial solutions to a triple $z$ representing solutions that satisfy all requisite constraints, the problem asks that we find the minimal such solution, which is simply $\min \pi_P z \cup \pi_Q z$.

### Efficiency

If the graph is small enough, it can always be preprocessed. On the other hand, if the number of starting points is small and the trajectories sparse, this is not very efficient.

The final step where we unify partial solutions is quite egregrious in its complexity, needing at worst $\mathcal{O}(S^Z)$ time, where $S$ is the number of starting points and $Z$ is the maximum number of possible ending points in any trajectory beginning at a starting point.

## Giving up

Even though attempts to code this solution were nearly complete, I gave up because I was struggling against Guile's ergonomics to both achieve good performance and keep my code elegant and readable, which was not fun for me at all. For example, I always had to be aware of exactly what sort of container I was working with and all the quirks of their APIs, which had me constantly searching countless pages of the Guile manual. Perhaps I have been spoiled by other languages like C++ and Rust, which have excellent collections libraries (especially the latter, whose iterator module `std::iter` is without equal), and Haskell, whose typeclasses and other features make committing my algorithm to code extremely easy. (I found myself greatly missing `Functor` and `Maybe`, both which also have serviceable analogues in Rust). Besides, I had already solved it on paper, so this was merely a (rather painful) formality.

## The morality of problems and solutions

In any domain, when clearly defining a problem (as opposed to discovering a problem), a solution to that problem should tautologically be *for* the problem. For purely algorithmic programming problems, one also wants desirable properties of the solution, such as good best/average/worst case time/space complexity.

Implicit in these desired properties is the input space the cases are drawn from, and that good solutions should work well on most or all of the possible inputs. Advent of Code problems seem to cleanly fall under the framework of algorithmic programming problems: a problem description, an expectation that you write a solution to the problem, and a large input that is used to verify you (probably) have a solution. (The alternatives, formal verification or running the program on hidden inputs, are respectively absurd or likely too costly for the authors.) However, in Advent of Code 2023 day 8 part 2, the solution drastically simplifies if one takes into account properties of the particular input in the problem statement. This violates the principle that the solution should work well on *most/all* inputs, not just the one input.

One could argue that most problems in practice (i.e. ones engineers encounter) are initially poorly defined and often require research and investigation to better define them. Under this view, engineering problems require sufficient investigation to deliver better and more cost-efficient solutions. From this point of view, Advent of Code problems do not ask you to write an algorithm to solve the described problem, but instead ask you to produce the correct output for the one given input. Indeed, this is exactly what is asked of you.

In my experience, however, this is not how most Advent of Code problems typically work. They are not the sort of engineering problems that software engineers typically spend their time solving. They are problems that require one to devise an algorithm &mdash; more of a mathematics or computer science problem than an engineering problem. I'm perfectly fine with solving engineering problems, but the appearance and history of Advent of Code leads one to believe that the offered problems are of the algorithmic type, not of the engineering type.

Furthermore, if one were to solve an Advent of Code problem with a quick-and-dirty script in a browser console, this would quite arguably not be in the spirit of the puzzle. Yes, this method literally solves the problem &mdash; getting the output for the singular input &mdash; but it corrodes the purity of the exercise. One is no longer writing clever solutions to precisely defined problems, but is instead merely chasing the goal of completion, of golden stars. At the risk of sounding silly, you can't sit back and know that you've written a solution that physically embodies the universal structures of algorithms and computation.

And that knowledge, that feeling of understanding a timeless problem and *proving it* by committing it to code, is pretty fun.
