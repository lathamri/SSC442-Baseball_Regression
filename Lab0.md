

## Technical Prerequisites

The following is a list of required actions you must take before the second class. In general, please bring your own laptop to class unless instructed otherwise. Ideally, you would install the below items **before** attending the second class. The course will flow much better if you are on top of the technical requirements. If you get stuck, read the "What to do if I can't complete a step?" section below. When in doubt, make sure you do the steps below in the order they are written; this may require that you start back at the beginning. If that doesn't solve the problem, this is a good time to learn about your best friend `Google`. Seriously, Google it.

### Setup Steps

1. Install `R` from https://cran.r-project.org

2. Install `RStudio Desktop` (Open Source License) from https://www.rstudio.com/products/rstudio/download.

3. `Optional` Install `Atom` (Open Source License) from https://atom.io. If you choose to take this route, you will need to take a few extra steps to get `R` to operate in the environment. But there is a major upside to this approach--particularly  if you program in multiple languages.

4. Register an account at https://github.com. This step is absolutely mandatory.

5. In this step you install the tidyverse packages, which includes `ggplot2`, the plotting library that we will use in this course. Enter the following commands in the R console (bottom left panel of RStudio) and make sure you see a plot in the bottom right panel and no errors in the R console:

```r
install.packages('tidyverse')
library(ggplot2)
ggplot(diamonds, aes(cut)) + geom_bar()
```

The remaining steps will allow you to install `git` and use it with RStudio.[^1]

[^1]: These steps will be remarkably different if you are utilizing Atom (for the better). This is one reason to use Atom as your editing and analysis environment. If you use Atom, you will need to become familiar with its various "packages". I strongly recommend that you try out `Hydrogen` and `atom-language-r` packages to allow you to execute `R` code in-line. A quick google search will provide the steps you need to get this set up. 

5. Bookmark, watch or star this repository so that you can easily find it later

6. Install `git` from https://git-scm.com/

7. Verify that in RStudio, you can see the path of the `git` executable binary in the Tools/Global Options menu's "Git/Svn" tab -- if not, then you might have to restart RStudio (if you installed git after starting RStudio) or installed git by not adding that to the PATH on Windows. Either way, browse the "git executable" manually (in some `bin` folder look for thee `git` executable file).
8. Create an RSA key (optionally with a passphrase for increased security -- that you have to enter every time you push and pull to and from GitHub). Copy the public key and add that to you SSH keys on your GitHub profile.
9. Create a new project choosing "version control", then "git" and paste the SSH version of the repo URL for this class copied from GitHub in the pop-up -- now RStudio should be able to download the repo. If it asks you to accept GitHub's fingerprint, say "Yes".
10. If RStudio/git is complaining that you have to set your identity, click on the "Git" tab in the top-right panel, then click on the Gear icon and then "Shell" -- here you can set your username and e-mail address in the command line, so that RStudio/git integration can work. Use the following commands:

    ```bash
    $ git config --global user.name "Your Name"
    $ git config --global user.email "Your e-mail address"
    ```
    Close this window, commit, push changes, all set.

Find more resources in Jenny Bryan's "[Happy Git and GitHub for the useR](http://happygitwithr.com/)" tutorial if in doubt or [contact me](mailto:bbushong@msu.edu). But please reserve the last option as a Last Resort.

### I Can't Complete A Step!

You should try the following strategies in the order they are written:

#### 1. Use `RStudio`'s built in help

If you don't know how to use a certain function, use R and RStudio's built-in help features. Generally speaking, `R` is a well-documented language. Remember, the computer only does what you ask it to do (literally). It's highly unlikely that it is flawed -- you are the flaw.

#### 2. Search the Internet

Arguably, searching the Internet (aka Googling) should be your first step for any technical help. Sometimes using built-in help will make solutions apparent, but quite often you will find opaque and confusing language greeting you after a "help" call. In such times, Google is your friend. Long after this class is over and when we -- Prof. Bushong and the course TAs -- won't respond to emails to help you, the Internet will be alive and well. Learning to solve your own issues will serve you for the long term, while asking us for help will not. Sites that are particularly likely to be helpful:

- Stackoverflow
- RStudio website
- Atom discussion boards / website (if using Atom)
- GitHub discussion boards

#### 3. GitHub Wiki

This course is intended to be collaborative. Sadly, there aren't great resources for collaboration without using lots of different software. Given that the class is designed to teach best practices for modern data analytics, we will utilize GitHub's built-in suite for discussions. This can be found on the course homepage https://github.com/MSU-DataAnalytics/SSC442. Critically, collaboration is a two-way street: make sure to respond to others' questions that you know how to solve.

You can directly solicit help from me on the Wiki, and I will answer more questions over the Wiki than I will over email. This allows me to provide one answer to dozens of people, rather than repeating information.


#### 4. Email

If you are uncomfortable posting your question to the whole class, you can message me directly via email.

I will however strongly discourage this as the term progresses *if* the reason that you don't want to post is that you simply feel it should be obvious or easy. Programming has lots of obscure corners, and I regularly spend anywhere between 5 minutes and 5 hours on installing or configuring various bits of software. Recently it took me 2 hours to realize that the error I received trying to switch from http to https (from unencrypted to encrypted internet traffic, essentially) was due to me having forgotten to switch off a firewall. In between, I had found 3 other possible 'solutions' that involved installing various tools, changing parts of the database, and so on. In the end, it was solved by running:

```bash
$ sudo ufw allow https
```
...yet it had wasted many hours of my time. Along the way, I learned some new things, and I generally felt more comfortable than when I began.

Thus, I discourage emailing for technical issues. Of course for non-technical questions or concerns, you are welcome to use email. And ultimately I don't want you to waste your time, so if you are totally stuck, please email me rather than repeatedly trying in vain to solve your problem.
