# Overlay Takeouts

## Abstract
- most known takeouts base on a simple cascade with right handed single passes
- we propose a way to base takeouts on a wide range of siteswaps

## Introduction
- _prolonged version of abstract_
- _give references to explain the historic background_
- _tell how we differ from and continue that history_
- _one sentence solution introduction_

The term "Takeouts" typically refers in juggling to one or more jugglers doing a pattern while an extra juggler (manipulator or middle-person) interferes with that pattern, often by introducing extra objects. This style has been known at least as far back as [Charlie Dancey's Compendium of Club Juggling](https://www.amazon.com/Charlie-Danceys-Compendium-Club-Juggling/dp/1898591148). An important impulse leading to contemporary takeouts were probably the [Take That Out performance](https://vimeo.com/28502455) and its sequel [Get The Shoe](https://www.youtube.com/watch?v=Yemkg_z7MAE). Takeout enthusiasts have generated an enormous variety of complicated and beautiful patterns, many of them collected in [Aidan Burns' famous 'How to steal from your friends'](http://www.geocities.ws/aidanjburns/passing.html). One of favorites of this style is [Bruno's Ace](https://www.youtube.com/watch?v=49Z7-wo_XtI) - Brunos' Nightmare with two rotating manipulators.

However all these variations boil down to three club cascades with sync 4-count passes manipulated with substitutions, intercepts and carries, as [Warrens article on Scrambled Passing Patterns explains](http://ezine.juggle.org/2014/05/26/scrambled-passing-patterns-and-takeout-notations-part-1/), some 2-, 3- and 5-counts being the exception.

Another line of developing new interactions between jugglers is based on [Christophe Prechac's article on Symmetric Passing Patterns](https://www.passingdb.com/articles.php?id=13). He essentially extended the generative power of siteswaps to the passing world. This technique has been extensively investigated and evangelized by the [Gandinis](http://www.gandinijuggling.com), leading to Sean's well known [Prechac explanation and lists](http://www.owenreynolds.net/notation/Symmetric_patterns_C.pdf) and ultimately to the epic [Social Siteswaps DVD](https://www.youtube.com/watch?v=W_G74eLnK1U). A practical tool for to generate these patterns is our [PrechacThis pattern generator](http://prechacthis.org), find e.g. various takeout inspired Prechac Siteswap patterns in the [Under Prechac Routine](http://underprechac.de).

Yet, that branch is focused on symmetrical patterns, i.e. every juggler is doing the same job, where as one very intriguing aspect of takeouts is the manipulation, i.e. someone is doing _something to_ the pattern.

Our current idea was to rejoin these two lines of development i.e. manipulators running around, but remove restrictions on the patterns. More specifically, instead of only three different versions of interactions between middle-person and passers and mostly four-count based passing, we look at period 3, 4, 5, 6 also with multiple passes, and allow "any" self (0, 1, 2, 3, 4) and pass (1p, 1.5p, 2p, 2.5p, 3p, 3.5p, 4p, 4.5p).

The technique we explored to bring these two lines together is: Take a prechac passing pattern and _overlay_ it with a custom made manipulation siteswap.

In the next section we introduce such a new pattern as an example to give a quick start to the broad idea. The section is followed by an deeper and more general explanation of the approach. By exploring a further example, with more of the intended features, we will try to bring across the advantages that we sought in this new technique.

## Motivating Example
- _start with a boring standard take out pattern (round-about) pointing out the missing ways to get to something more interesting_
- _motivating example: delightfull_
- _explain trick, show what's opening up here_
One well established pattern is the so called roundabout (TODO video link): a 6 club 4-count with middle-person substituting passes and selves, where the middle-person swaps roles with a passer who will then become the new middle-person and so on. Let's see how we can add prechac siteswaps to roundabout like patterns by a short tutorial on a pattern that was accidentally named "delightful". We want to start out with a simplistic proof of concept, so we choose to have not too many clubs for the passers (i.e. 5) and only one extra club for the middle-person. Also the passing pattern should not be too demanding so let's go for a period 4 passing pattern: [5 clubs, period 4](http://prechacthis.org/index.php?persons=2&objects=5&lengths=4&max=3&passesmin=1&passesmax=4&jugglerdoes=&exclude=&clubdoes=&react=&results=). 
- _add material from here:_ https://github.com/prechac/prechacthis/blob/a5bfcf09f936b70f18f1a9309e8a2ed3765d73da/2016-11-overlay-takeouts.md#generating-overlay-takeouts-1


## The General Principle
- _summarize the idea based on the example_
- _(1) take/create a prechac pattern, that has one self > 1+period/2_
- _(2) create solo siteswap of the same period containing that self -> but transform the self into a pass_
- _(3) juggle both at once_
- _(4) add some walking sequence to it_

## Fully fledged example Mission Impossible
- _What where we aiming for?/Requirements:_ 
    - _not too demanding for participants (throws, catches, number of clubs, "familiar" elements)_
    - _left right handed, i.e. odd period_
    - _possibility to correct a wrong handed 1.5p, e.g. club does 1 1.5p_
- _First approach: 1 2 3 4 2.5p and 0 1 2 1.5p overlayed to: 1 2 3 1.5p 2.5p_
- _take material from here:_ https://github.com/prechac/prechacthis/blob/a5bfcf09f936b70f18f1a9309e8a2ed3765d73da/2016-11-overlay-takeouts.md#going-forward---period-5

## Observations, Discussion
- _Point out that it is, at first, surprisingly hard to juggle the overlay, even though on masters both partial patterns_
- Yes, the passer cannot ignore the manipulator, but that is true for most takeouts ("pelf")
- _Once you by pass the your thinking machinery, its actually not that hard, you become part of a clock work, which is what the whole takeout business is about_
- _Two step approach: (1) creating overlay prechacs, (2) dealing with runaround pecularities_

## Future Work
- _Describe the run around part of Mission Impossible in more detail (transition, starts, permutations)_
- _let's see how many interesting patterns can be generated that way_
- _See how Aidan's and Ed's notation can help to find a theory for the runaround part_