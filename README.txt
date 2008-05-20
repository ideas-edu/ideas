          Intelligent Feedback Project -- Open Universiteit Nederland

Copyright 2008, Open Universiteit Nederland. The source code of this project is 
distributed under the terms of the GNU General Public License. For more information, 
see the file "LICENSE.txt", which is included in the distribution.

----------------------------- Installation guide -----------------------------

Currently, there are no binary packages available, and the feedback tools have to be build from source. 

Requirements:
=============

- Glasgow Haskell Compiler version 6.8.1
     Sources and binary packages for most operating systems can be found at:
        http://www.haskell.org/ghc/download_ghc_681.html
     Important: this is not the most recent version of the compiler.

- Support for makefiles 
     On Windows machines, cygwin should be sufficient, which can be downloaded from:
        http://www.cygwin.com/

- The Haskell Parser Combinator Library from Utrecht University (uulib)
     This library is included in the zip-files, and the sources have been modified to
     work under ghc-6.8.1. No extra work has to be done.
     The original sources can be obtained from http://www.cs.uu.nl/wiki/HUT/WebHome

How to build:
=============

- Download the sources from the project page (http://ideas.cs.uu.nl/trac). This page 
  has prepared zip-files with snapshots of the code. An alternative is to get the 
  sources from our svn server.

- Go to the root directory (with the makefile) and type:
     
     make laservice
  
  This will build the file laservice.cgi in the bin directory. This file is a "normal"
  executable, with some support for off-line testing. The following commands are useful
  to test the proper working of the feedback service:
     
     bin/laservice.cgi --test test/mathdox-request/ok.txt
     bin/laservice.cgi --test test/mathdox-request/incorrect.txt

How to install:
===============

- Just copy the file bin/laservice.cgi to the cgi-bin directory of your web-server. Note
  that all requests and replies are automatically logged in a file called laservice.log.
  Because the protocol is somewhat verbose, this log file quickly becomes quite large.

Contact:
========

   Bastiaan Heeren   (bastiaan.heeren at ou.nl)
   Johan Jeuring     (johanj at cs.uu.nl)
   Alex Gerdes       (alex.gerdes at ou.nl)

------------------------------------------------------------------------------