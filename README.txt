XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)
====================================================

0. Overview
-----------
This package contains implementations of a XML-RPC server
and a XML-RPC client. Target platforms are Windows, Linux
and native DOS by use of DWPL and WDOSX. Demo projects for
server and client implementations are included.


1. Copyright
------------
(c) 2001-2003 by Team-DXmlRpc
  e-mail: team-dxmlrpc@dwp42.org
  www:    http://sourceforge.net/projects/delphixml-rpc/

The initial developer of the package is:
  Clifford E. Baeseman, codepunk@codepunk.com


2. Contributors
---------------
  S. R. Oddson,
  Patrick Martin,
  Avi Lewin,
  Alessio Pollero (alessio.pollero@yandex.com),
  Immo Wache (immo.wache@dwp42.org)


3. Bugs and feature requests
-----------------------------
Please supply all bug fix requests to:
  team-dxmlrpc@dwp42.org
or visit:
  http://sourceforge.net/projects/delphixml-rpc/


4. Licence
----------
This package may be distributed and/or modified under
the terms of the GNU Lesser General Public License
(LGPL) version 2.1 as published by the Free Software
Foundation. See the included license.txt file or visit:
  http://www.dwp42.org/license/lgpl.html


5. Used libraries
-----------------
This library contains a mime encoder by
Ralf Junker <ralfjunker@gmx.de> 2000-2001
  http://www.zeitungsjunge.de/delphi/ 

This library also contains the excellent xml parser
written by Stefan Heymann.
  http://www.destructor.de

The complete used library packages are included in
folder xmlrpc\contributions.


6. Installation
---------------
Unzip this package (and keep the folder structure) 
into any folder you like, e.g.
  C:\program files\borland\delphi7\xmlrpc

Open the project group in xmlrpc\build\buildall.bpg
and build all (menu item Project|Build all projects).

Check the examples in xmlrpc\demos and xmlrpc\tests.

For native Delphi Windows mode and Kylix you must
install the latest INDY components to compile!
  http://www.nevrona.com/Indy/

Note that unless installed by hand in Delphi7, Kylix3
the builtin INDY version is 9 while for newer versions
of Delphi(like XE) the builtin version is 10, 
so make sure that in the indy.inc file you have defined 
the right version of INDY based on your environment.

For DWPL Delphi WDOSX for DOS mode you need the 
latest DWPL library from
  http://www.dwp42.org

To install on Lazarus, the Indy10 Lazarus/FPC port
is required, for more information : 
  http://www.indyproject.org/sockets/fpc/
and then install dxmlrpc.lpk package.

7. Change Log (from 2.0.0)
--------------------------
The new change log is availabe in file changelog.


8. Old Change Log (before 2.0.0)
--------------------------------
The old change log until release 1.7.7 is available
in file README.txt included in package xmlrpc1_7_8.zip
in the folder xmlrpc\contributions.


9. Useful links
---------------

http://www.xmlrpc.com/
http://www.commercemechanics.com/index.php?object=downloads
