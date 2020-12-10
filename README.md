# hypothesis-testing
Does some of the steps for conducting a hypothesis test using the binomial distribution, you still have to determine the null and alternative hypothesis yourself.  
Not the cleanest code I've ever written but it hopefully is somewhat understandable  
LIMITATION: Only the half of the critical region that is relevant to the inputted test stat is shown when performing a two-tailed test

You can run without installing Scala on your machine by going to https://repl.it/@rorysharp/AttachedExhaustedGame, clicking the run button and being patient

Feedback and pull requests are welcome

## Screenshots
![A test for an increase](increase_demo.png?raw=true "A test for an increase")

![A test for an decrease](decrease_demo.png?raw=true "A test for an decrease")

![A two-tailed test](two_tail_demo.png?raw=true "A two tailed test")

## Copyright
Copyright © 2020  Rory Sharp All rights reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

You should have received a copy of the GNU General Public License
along with this program.  If you have not received this, see <http://www.gnu.org/licenses/gpl-3.0.html>.

For a (non-legally binding) summary of the license go to https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)
## Installation
### Prerequisites
* Scala
* Scalac (optional)
### Process
0\. Download this repo (`git clone https://github.com/qwertpi/hypothesis-testing.git && cd hypothesis-testing`)  
1\. Either run as a script (`scala main.scala`) or compile (`scalac main.scala`) then run the compiled code (`scala HypothesisTesting`)
