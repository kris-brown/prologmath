# prologmath

As a programmer studying mathematics (undergraduate abstract algebra), I often am faced with the challenge "Given X, prove Y, using any of the facts you've seen in the book thus far". Let's say the structure of a 'fact' is "Given A, one can prove/construct B" and a knowledge base (KB) is a list of such facts. Automated theorem proving seeks to find the proof given KB, but I am not interested in that. My more modest goal is to just filter KB to identify facts that are possibly relevant for the task at hand. I think of this as a **unification** problem: given an X at hand, identify facts where X matches A (alternatively, pattern match on the result to work backwards, or use both as constraints at once).

This could only give me candidates for the first step in a proof, but I think it would be a helpful tool. In summary, I'm seeking a principled way to take math notes that allows me to query what I've written down in a useful way.

To show a concrete example of what I'm talking about, I tried a naive implementation in Prolog using the first chapter of my algebra textbook. Prolog programs are just the declaration of facts, but this program has an explicit relation called "fact" (name, page number, input pattern, output pattern, description). There are also normal mathematical facts, so that when making an input pattern for a "normal subgroup of 6 elements" that theorems relevant to groups (in general) will be matched. Maybe in another language it would be possible to unify the declaration of both of these.

```
?- facts([1,2]).

> **************************************
> OddSum (Page 22)
> Sum of odd and even number is odd

% this only will be returned if a pattern is given with an even and an odd number (any order)

?- facts([z,n]). % Relies on having defined z and n as sets (regular prolog declarations)

> **************************************
> Constructions from two sets (Page 342)
> Given any two sets, one can construct the UNION set and INTERSECTION set

?- facts([z,n],2). % Relies on having defined z and n as sets (regular prolog declarations)
> [nothing] % This "fact" is the construction of a set from two sets, and by giving
            % an integer pattern for our result pattern match, we no longer got
            % the previous result
            % To match an arbitrary set, one can construct one (create an atom and
            % specify nothing about it other than that it is a set) or just pick
            % one, such as z.

```

What work has been done on this front? The relevant search term seems to be "Mathematical Knowledge Management" but my limited literature search in that field didn't reveal find anything as restricted as the problem I describe (text search through 500+ pg review on MKM gave no interesting hits for "unification" or "pattern match").

Unification seems like a place where logical programming would excel in. I suspect first order unification would be sufficient for most of the things I wish to encode (if I don't mind writing some boilerplate), but that might change. I also feel like, with the subject matter of abstract mathematics, a strong type system would also be helpful for pruning the search space, especially given that I want to store a large number of facts. This led me to consider Mercury, although I'm hesitant to try picking up a language that has such a small community. Outside of that, I feel like Prolog could be appropriate, or a unification implementation in some language I'm more comfortable with (e.g. Haskell, Java, Python). So if this problem has not been solved already, I would appreciate feedback for a language in which to try addressing it.
