# LLLang
A Linear Logic based programming Language (LLL).

Linear logic is a formal system wich can models the consumption and creation of ressources. In a well-typed linear program,
there are neither deadlocks nor data races with concurrential processes, and a socket or file is never written to or read from
once it is closed, and are closed after their used. Linear typing force ressources to be consumed exactly once.

The Rust language uses a weaker form of this called affine typing where it ensures that memory zones have at most one owner at
the same time, which dismissed completely a whole class of bugs in safe Rust, like use after free, double free or data races.

Currently, I'm struggling with writing the formal definition of LLL, but I already wrote some WIP code for the interpreter. 

# References
* [Computational interpretations of linear logic](https://www.sciencedirect.com/science/article/pii/030439759390181R) by Abramski
* [Fundamental of session types](https://www.sciencedirect.com/science/article/pii/S0890540112001022) by Vasconcelos
* [Propositions as Sessions](https://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-sessions/propositions-as-sessions.pdf) by Wadler
