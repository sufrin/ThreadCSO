# Examples of concurrent programs using multiple workers

All the programs here use multiple workers to achieve jobs collectively.
Some use a bag-of-tasks protocol (possibly "distributed") to share 
the tasks entailing a job among workers. Others share the work of
an essentially-cellular matrix algorithm among workers that take responsibility
for distinct parts of the matrix. All programs are worth studying (perhaps in 
conjunction with my lecture notes).

## Programs using a Bag of Tasks protocol

- EightQueens

This program explores the space of solutions to the eight-queens 
puzzle


---

- MagicSquares
- MagicSquaresOpt
   
These programs both use the ordinary bag of tasks protocol.
They search the solution space of *magic squares* of a certain size,
and print an asterisk (or the solution itself) when they find a
solution. A worker in the `Opt` version doesn't explore any partial
solutions that are already being explored by other workers.

For sizes above 4 it can take a while before magic squares appear. The program shows that it's
still working by printing `.` from time to time.

         ./run MagicSquaresOpt -p -w=5 -s=5
        .............................
        03  17  16  25  04
        11  06  20  07  21
        24  09  18  02  12
        05  14  10  23  13
        22  19  01  08  15
        .......................................................................
        03  17  16  25  04
        11  06  20  10  18
        12  07  23  02  21
        24  13  01  19  08
        15  22  05  09  14
        ....
 
---

- DistBagSquares 

This program uses the distributed bag of tasks protocol. It prints magic
squares of a specified size (4 by default). 

---

- Occurences

This program makes a concordance of the occurences of "words"
matching a given pattern among a collection of files identified by
their path names or their URLs. The pattern that identifies a word
can be specified as a regular expression, as can rods to include
or exclude. Specifying a word-pattern with (`-w`) makes possible some
useful source-text processing. For example:  

        -w "(class|trait|def)[ ]+" 
        
indexes all class, trait, and function/method definitions.

---

- Digest
- Digesticator
  
These programs both *digest*  the texts in a collection of files
identified by their (local) paths or URLs.  Digesting  counts
occurrences of individual words. The main point of the program is
to demonstrate that there is some advantage in allocating more
processes than there are processors to a task that is input/output
bound. This is demonstrated most effectively when digesting a
sequence of files identified by (remote) URLs.
  
  
Here is a trivial "benchmarK" that compares performance 
of the programs on the scala files in the source
directory. -r is the number of readers; -d is the number of
digesters.

        1144 % ./run Digesticator  `findscala`
        -r 1 -d 1: 00.108,627 (17 files, 1043 words / 1157 digested)
        1145 % ./run Digesticator -r 5 -d 2 `findscala`
        -r 5 -d 2: 00.094,383 (17 files, 1043 words / 1157 digested)
        1146 % ./run Digesticator -r 5 -d 4 `findscala`
        -r 5 -d 4: 00.089,072 (17 files, 1043 words / 1157 digested)
        1147 % ./run Digesticator -r 5 -d 4 `findscala`
    
    
(Note added July 2021: I cannot remember what the differences between the digest programs is.)

---

- Crawl 

A prototype Web-Crawler. This program implements a web-crawler using
the straightforward bag-of-tasks architecture. It is advisable to
reqire that it limits its crawl to a specific site, otherwise it
may well not terminate in the time you have available.

        1148 % ./run Crawl -s=https://www.cs.ox.ac.uk/people/bernard.sufrin \
                     https://www.cs.ox.ac.uk/people/bernard.sufrin/

It accumulates edges in the graph rooted at the given URL, limiting
its exploration to the site, if one is specified by `-s=`*url*. As
it starts to explore each new node, the program prints its URL. As
each edge is discovered the program prints the source and destination
URL.  When (if ever) its exploration of the graph is complete it
prints the accumulated sequence of URLs (in no particular order).

- WebCrawl
- WebCrawler
These programs were used to test the algorithm for `Crawl` on simulated
networks of URLs. 

---

## Programs using Barrier Synchronization

- Labelling

A program that labels connected components of an "image" (represented as "ascii art").


    
    
