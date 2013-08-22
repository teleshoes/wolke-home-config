#!/usr/bin/env python

#ROOTSTRAP_URL = "http://harmattan-dev.nokia.com/d6.php?f="
ROOTSTRAP_URL = "file:///home/wolke/Desktop/Software/scratchbox/"
'''
Setup script for the Harmattan SDK

Author: Ruslan Mstoi
'''

#       (C) Copyright 2007-2011 by Nokia Corporation. All rights reserved.
#
#       Contact: I_EXT_SDK_BUGZILLA@nokia.com
#
#       Permission is hereby granted, free of charge, to any person
#       obtaining a copy of this software and associated documentation
#       files (the "Software"), to deal in the Software without
#       restriction, including without limitation the rights to use,
#       copy, modify, merge, publish, distribute, sublicense, and/or sell
#       copies of the Software, and to permit persons to whom the
#       Software is furnished to do so, subject to the following
#       conditions:
#
#       The above copyright notice and this permission notice shall be
#       included in all copies or substantial portions of the Software.
#
#       THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#       EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#       OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#       NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#       HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#       WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#       FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#       OTHER DEALINGS IN THE SOFTWARE.


# $Id: harmattan-sdk-setup.py 2102 2012-01-25 09:37:57Z rmstoi $

import BaseHTTPServer
import subprocess
import threading
import sys
import os
import time
import shutil
import pwd
import textwrap
import urllib
import cStringIO
import ConfigParser
import traceback
import platform
import getpass
import tarfile
import urlparse
import signal
import tempfile
import re
import grp
import netrc

from optparse import OptionParser

# names shown to the user (what is installed/removed and this scripts name)
PRODUCT_NAME = "Harmattan SDK"
PRODUCT_NAME_SHORT = "SDK"
# used to name created files
PRODUCT_NAME_FILES = PRODUCT_NAME.replace(" ", "-").lower()

MY_NAME = '%s Setup' % PRODUCT_NAME
MY_VERSION = "0.2.1 HARMATTAN ($Revision: 2102 $)"

# the location of the script, to check for updates
MY_URL = "TODO"

# this is where the log file resides and all the downloaded files are saved to
DIR_TEMP = "/tmp"

# where to install the executable files
DIR_BIN = "/usr/local/bin"

# for the UI
SB_NAME = 'Scratchbox'

# Where scratchbox is found and is going to be installed to in case of tarball
# installation. This path must begin with / and end with /scratchbox, check
# check_sb_path_sane for details.
SB_PATH = "/scratchbox"

# scratchbox group
SB_GROUP = "sbox"

# where scratchbox tarballs are
SB_TARBALL_URL = "http://scratchbox.org/download/files/sbox-releases/hathor/tarball/"

# the names of the Scratchbox tarballs
SB_TARBALL_FILES = [
    "scratchbox-core-1.0.27-i386.tar.gz",
    "scratchbox-libs-1.0.27-i386.tar.gz",
    "scratchbox-devkit-qemu-0.13.90-0rabbit1-i386.tar.gz",
    "scratchbox-devkit-debian-squeeze-1.0.7-i386.tar.gz",
    "scratchbox-devkit-hashutils-squeeze-sdk-1.0.12-i386.tar.gz",
    "scratchbox-devkit-perl-1.0.5-i386.tar.gz",
    "scratchbox-toolchain-host-gcc-1.0.27-i386.tar.gz",
    "scratchbox-toolchain-cs2009q3-eglibc2.10-armv7-hard-1.0.24-18-i386.tar.gz",
    "scratchbox-toolchain-cs2009q3-eglibc2.10-i486-1.0.24-12-i386.tar.gz"
]

SB_DEB_REPO = "deb http://scratchbox.org/debian harmattan main"

# the names of the Scratchbox packages on Debian based systems
SB_DEB_PACKAGES = [
    "scratchbox-core", 
    "scratchbox-libs",
    "scratchbox-devkit-qemu",
    "scratchbox-devkit-debian-squeeze",
    "scratchbox-devkit-hashutils-squeeze-sdk",
    "scratchbox-devkit-perl",
    "scratchbox-toolchain-host-gcc",
    "scratchbox-toolchain-cs2009q3-eglibc2.10-armv7-hard",
    "scratchbox-toolchain-cs2009q3-eglibc2.10-i486"
]

# where to put the downloaded Scratchbox packages
SB_FILE_DIR = DIR_TEMP

# the emulator
EMU_NAME = "QEMU"

# SDK installation envienvironment, for processes running under Scratchbox
# (e.g. apt)
SDK_INSTALL_ENV = {
    # Debian frontend to noninteractive
    "DEBIAN_FRONTEND" : "noninteractive",
    "DEBIAN_PRIORITY" : "critical",

    # don't start stuff

    "SBOX_REDIRECT_BINARIES" :
        # fake start-stop-daemon
        "/sbin/start-stop-daemon:%(sb_path)s/tools/bin/true,"
        "%(sb_path)s/devkits/debian/bin/start-stop-daemon:%(sb_path)s/tools/bin/true"

        # fake invoke-rc.d 
        "/usr/sbin/invoke-rc.d:%(sb_path)s/tools/bin/true,"
        "%(sb_path)s/devkits/debian/bin/invoke-rc.d:%(sb_path)s/tools/bin/true,"

        # fake install-info 
        "/usr/sbin/install-info:%(sb_path)s/tools/bin/true,"
        "%(sb_path)s/devkits/debian/bin/install-info:%(sb_path)s/tools/bin/true,"

        # fake dpkg-divert
        "/usr/sbin/dpkg-divert:%(sb_path)s/tools/bin/true,"
        "%(sb_path)s/devkits/debian/bin/dpkg-divert:%(sb_path)s/tools/bin/true,"

        # fake update-alternatives 
        "/usr/sbin/update-alternatives:%(sb_path)s/tools/bin/true,"
        "%(sb_path)s/devkits/debian/bin/update-alternatives:%(sb_path)s/tools/bin/true"
        % {"sb_path" : SB_PATH},

    # without this sb throws: "ERROR: Cannot determine user. $USER is
    # null. Please check your environment."
    "USER" : os.environ["USER"]
}

# maintainer contact info
CONTACT_NAME = "Nokia"
CONTACT_EMAIL = "I_EXT_SDK_BUGZILLA@nokia.com"
CONTACT_BUGS_URL = "developer.nokia.com/bugs"

# SDK time & space consumption: these are very rough
INST_CONS_TIME = '20 minutes'
INST_CONS_SPACE = "2GB"

# command line options and arguments
OPTIONS = None
ARGS = None
OPT_PARSER = None

# possible arguements for the user interface choice option
CHOICE_UI_QT = "q"
CHOICE_UI_CL = "c"

# global objects:
# messenger, initialized later on, after root access is verified
# because log file can exist already with root permissions
MSGR = None
IMPORTER = None
PKG_MGR = None
CREDS_MGR = None

# environment variable that must be set to start UI testing, when UI is being
# tested all the pages will be shown and tasks won't run
TESTING_UI_ENV_VAR  = "HARMATTAN_SDK_SETUP_TESTING_UI"
TESTING_UI = False

# PIDs of the currently running children
CHILD_PIDS = set()

# some choices used in multiple functions
ASK_CHOICE_YES = "y"
ASK_CHOICE_NO = "n"
ASK_CHOICE_ABORT = "abort"
ASK_CHOICE_RETRY = "retry"

class Target(object):
    '''Contains default target data. Any user modifications to this data (like
    new target name prefix) are stored in UsersSelections.'''

    # things that are common for all targets are class attributes

    # default target names prefix, user can choose different prefix if default
    # targets already exist in the Scratchbox
    name_prefix = "HARMATTAN"

    toolchain_prefix = "cs2009q3-eglibc2.10"

    rootstrap_base_url = ROOTSTRAP_URL
    rootstrap_suffix = "public-sdk-rootstrap.tgz"

    # downloaded rootstrap will be saved into this directory
    rootstrap_dir = DIR_TEMP

    rootstrap_url_is_magic = True

    def __init__(self, name_postfix, toolchain_postfix, rootstrap_arch,
                 devkits, cputransp):
        '''Constructor.
        name_postfix = postfix to identify this target, prefix is the same for
                       all targets
        toolchain_postfix = postfix of toolchain to be used with this target
        rootstrap_arch = rootstrap architecture, will be used to determine
                         the full rootstrap path
        devkits = colon separated string of devkits for this target
        cputransp = CPU transparency method to be used with this target'''
        self.name_postfix = name_postfix
        # this is the default target name, if this target exists the user has
        # the choice to specify an alternative name
        self.name = self.name_prefix + self.name_postfix 
        self.toolchain = self.toolchain_prefix + toolchain_postfix 
        self.rootstrap_arch = rootstrap_arch
        self.devkits = devkits
        self.cputransp = cputransp

        rootstrap_basename = "%s-%s" % (rootstrap_arch , self.rootstrap_suffix)

        # URL to download the target rootstrap from
        self.rootstrap_url = ("%s%s" %
                              (self.rootstrap_base_url, 
                               rootstrap_basename))

        # where to save the rootstrap on the host
        self.rootstrap_fn = os.path.join(self.rootstrap_dir,
                                         rootstrap_basename)

    def setup(self, username, install_selections):
        '''Sets up a Scratchbox target.
        username = user for whom set up the target
        install_selections = user's selections'''

        # use alternative target name prefix if specified
        if install_selections.targets_prefix:
            target_name = install_selections.targets_prefix + self.name_postfix
        else:
            target_name = self.name

        setup_target(username, target_name, self.rootstrap_fn, self.toolchain,
                     self.devkits, self.cputransp, install_selections.proxy)

TARGET_X86 = Target(name_postfix = "_X86",
                    toolchain_postfix = "-i486",
                    rootstrap_arch = "i386",
                    devkits = "perl:debian-squeeze:hashutils-squeeze-sdk",
                    cputransp = "")
    
TARGET_ARMEL = Target(name_postfix = "_ARMEL",
                      toolchain_postfix = "-armv7-hard",
                      rootstrap_arch = "arm",
                      devkits = "qemu:perl:debian-squeeze:hashutils-squeeze-sdk",
                      cputransp = "qemu-arm-sb")

# SDK license text
EUSA_TEXT = '''IMPORTANT: READ CAREFULLY BEFORE INSTALLING, DOWNLOADING, OR USING THE LICENSED
SOFTWARE. 

NOKIA SOFTWARE DEVELOPMENT KIT AGREEMENT

This Software Development Kit Agreement ("Agreement") is between You (either an
individual or an entity), the Licensee, and Nokia Corporation ("Nokia"). The
Agreement authorizes You to use the Licensed Software specified in Clause 1.4
below, which may made available to You by Nokia on a CD-ROM, sent to You by
electronic mail, or downloaded from Nokia's Web pages or Servers or from other
sources, as determined by Nokia, under the terms and conditions set forth
below. This is an agreement on licensee rights and not an agreement for sale.
Nokia continues to own the copies of the Licensed Software, which is the
subject of this Agreement, and any other copies that You are authorized to make
pursuant to this Agreement.

Read this Agreement carefully before installing, downloading, or using the
Licensed Software.  By clicking on the "I Accept" button, or by typing "I
Accept" while installing, downloading, and/or using the Licensed Software, You
agree to the terms and conditions of this Agreement. If You do not agree to all
of the terms and conditions of this Agreement, promptly click the "Decline" or
"I Do Not Accept" button, or do not type "I Accept" and cancel the installation
or downloading, or destroy or return the Licensed Software and accompanying
documentation to Nokia. YOU AGREE THAT YOUR USE OF THE LICENSED SOFTWARE
ACKNOWLEDGES THAT YOU HAVE READ THIS AGREEMENT, UNDERSTAND IT, AND AGREE TO BE
BOUND BY ITS TERMS AND CONDITIONS.

1. Definitions

1.1 "Affiliates" of a party shall mean an entity (i) which is directly or
indirectly controlling such party; (ii) which is under the same direct or
indirect ownership or control as such party; or (iii) which is directly or
indirectly owned or controlled by such party. For these purposes an entity
shall be treated as being controlled by another if that other entity has fifty
percent (50 %) or more of the votes in such entity, is able to direct its
affairs and/or to control the composition of its board of directors or
equivalent body.

1.2 "Documentation" shall mean any documents, drawings, models, layouts and/or
any other IPR that are documented in any media and that have been provided by
Nokia or any of its Affiliates to Licensee.

1.3 "IPR" shall mean any and all artifacts and knowledge related to the
Licensed Software, to the extent either generated in connection with the
evaluation or use of the Licensed Software, incorporated and/or used in the
design or function of any result of such evaluation or use and/or developed
and/or discovered during such evaluation or use, including without limitation
the components, prototypes, documents, designs and computer software, as well
as ideas, inventions, patents (including utility models), and designs (whether
or not capable of registration), chip topography rights and other like
protection, copyrights (including software copyright of source code, object
code, executable code etc.), trademarks, know how, sui generis rights to data
or databases and any other form of statutory protection of any kind and
applications for any of the foregoing respectively as well as any trade
secrets.

1.4 As used in this Agreement, the term "Licensed Software" means,
collectively: (i) the software copyrighted by Nokia Corporation or third
parties (excluding Open Source Software), which is delivered to Licensee as
part of the Nokia Software Development Kit for the MEEGO-Platform (ii) all the
contents of the disk(s), CD-ROM(s), electronic mail and its file attachments,
or other media with which this Agreement is provided, including the object code
form of the software delivered via a CD-ROM, electronic mail, or Web page (iii)
digital images, stock photographs, clip art, or other artistic works ("Stock
Files") (iv) related explanatory written materials and any other possible
documentation related thereto ("Documentation"); (v) fonts, and (vi) upgrades,
modified versions, updates, additions, and copies of the Software (collectively
"Updates"), if any, licensed under this Agreement.

1.5 "Pre-Existing IPR" shall mean any IPR that has not been either generated in
connection with the evaluation or use of the Licensed Software, incorporated
and/or used in the design or function of any result of such evaluation or use
and/or developed and/or discovered during such evaluation or use.

1.6 "Third Party Software" shall mean software created by neither of the
Parties.

2. PERIOD AND SCOPE OF THE LICENSEE's PERMITTED USE
 
2.1 The Licensee shall be entitled to exploit the license granted to it by
Nokia under and in accordance with this Agreement solely for the purpose of
porting and developing software for the MEEGO-Platform (the "Purpose").

2.2 Nokia may choose to deliver, at its sole discretion, updates for the
Licensed Software. In addition Nokia may choose, at its exclusive discretion,
to make available, either as downloadable modules from a dedicated website or
as electronic deliveries via e-mail, to the Licensee the subsequent updates of
the Licensed Software as well as any subsequent directly related minor
upgrades, if any. As a prerequisite to the making available of any subsequent
upgrades and/or updates, Nokia may require, at its sole discretion, the
Licensee to accept additional terms and conditions in connection with the
delivery and prior to any use of any such subsequent upgrades and/or updates
made available to the Licensee by Nokia or its Affiliates. The Parties may also
separately agree on the licensing of any future releases of the Licensed
Software, if any, to be part of the Agreement by concluding a necessary
amendment to the Agreement and by agreeing on the respective terms and
conditions.

3. GRANT OF RIGHTS

3.1 Subject to the terms and conditions of this Agreement, Nokia grants to
Licensee, and Licensee hereby accepts, a non-transferable, non-sublicenseable,
non-exclusive, limited license to install and use Licensed Software on the
local hard disk(s) or other permanent storage media, copy, run and utilize the
Licensed Software in object code form solely for the Purpose. In addition,
Licensee may make one extra copy of the Licensed Software as an archival backup
copy. Any other copies made by the Licensee of the Licensed Software are in
violation of the Agreement.

3.2 Notwithstanding the generality of subsection 3.1 above and insofar as the
Licensee is provided with components of the Licensed Software in source code
form ("Source Code Components"), Nokia grants to Licensee and Licensee hereby
accepts, subject to the terms and conditions of this Agreement, a
non-transferable, non-sublicenseable, non-exclusive, limited license to compile
such Source Code Components, without any modification whatsoever, with Licensee
Application(s) solely for the Purpose and prepare copies of the Source Code
Components necessary for such compilation. For the sake of clarity, the
Licensee rights under this subsection 3.2 shall be subject to the limitations
set forth in Clause 4 below.

3.3 Parts of the Licensed Software may be supplied by third parties and may be
subject to separate license terms. The Licensee shall accept any such license
terms applicable to Third Party Software that may be introduced as part of the
installation of the Licensed Software.

4. LIMITATIONS OF LICENSE AND LOAN

4.1 Licensee shall have no right to disclose, sell, market, commercialise,
sub-license, rent, lease, re-license or otherwise transfer to any other party,
whatsoever, the Licensed Software or any part thereof, or use the Licensed
Software for any purpose or in any manner that is not expressly permitted in
this Agreement. 

4.2 Licensee shall not modify or develop any derivative works of the Licensed
Software unless specifically authorized by this Agreement. Licensee may not
reverse engineer, decompile, or disassemble any part of the Licensed Software,
except and only to the extent that such activity is expressly permitted by
applicable law notwithstanding this limitation.

4.3 The Licensee understands that the rights and licenses granted under this
Agreement with respect to the Licensed Software are granted subject to third
party intellectual property rights and as such may not be enough to use or
develop the Licensed Software or any application created with the Licensed
Software, and therefore the Licensee and any users may need to obtain separate
licenses for necessary technologies in order to implement the necessary
solutions or use the Licensed Software.

4.4 Licensee undertakes to have written personal agreements between Licensee
and its employees (if any) having access to the Licensed Software or any
Software created using Licensed Software or any related information for the
purpose of ensuring compliance with the terms and conditions of this Agreement.

4.5 Licensee undertakes not to disclose, license or allow any use of software
created using Licensed Software, outside the permitted Purpose, without express
prior written consent of Nokia.
 
4.6 Misuse of the Licensed Software or data generated by the Licensed Software
is strictly prohibited by Nokia, and may violate U.S. and other laws. Licensee
is solely responsible for any misuse of the Licensed Software under this
Agreement and Licensee agrees to indemnify Nokia and its Affiliates for any
liability or damage related in any way to Licensee's use of the Licensed
Software in violation of the same. Licensee is also responsible for using the
Licensed Software in accordance with the limitations of this Agreement.
Licensee will indemnify and hold Nokia and its Affiliates harmless for any
claims arising out of the use of the Licensed Software or data generated by the
Licensed Software on devices belonging to the Licensee or any of Licensee's
customers or any third party that has been provided access to the Licensed
Software or data generated by the Licensed Software, except to the extent those
claims arise out of the Nokia's breach of this Agreement.

4.7 There are no implied licenses or other implied rights granted under this
Agreement, and all IPR and rights, save for those licenses expressly granted to
the Licensee hereunder, shall remain with and vest in Nokia. Nokia shall retain
at all times a right to license, market, develop, modify and customize the
Licensed Software. The non-exclusive rights granted to Licensee under this
Agreement should not be construed as a limitation on any activities of Nokia or
any of its Affiliates in the use of the Licensed Software and the marketing or
licensing of the Licensed Software, and Nokia retains all of its rights, title
and interest in the IPR, the Licensed Software and in any derivative works
thereof. Except for the rights and licenses granted to Licensee herein,
Licensee shall not take any action inconsistent with such title and ownership.

5. FEEDBACK

Licensee may provide Nokia comments, suggestions, opinions and the like as
feedback on the characteristic, performance, functionality and use of the
Licensed Software (the "Feedback"). Feedback includes without limitation
materials as well as ideas or know how (whether presented orally, in written
form or otherwise). Any Feedback submitted by the Licensee or any of its
employees or subcontractors shall, after submission to Nokia, be owned by
Nokia. Nokia shall receive, upon submission of Feedback, a worldwide,
perpetual, irrevocable, fully paid-up, royalty-free, exclusive, transferable,
sub-licenseable license to use, copy, modify, make available, incorporate in
any products or services of Nokia, sublicense, distribute and otherwise make
use of the Feedback and to create derivative works thereof to the extent and in
such manner as Nokia deems fit at its exclusive discretion. For the sake of
clarity, the license granted to Nokia above in this Clause 5 shall include
rights and licenses to any Pre-existing IPR of Licensee deemed necessary by
Nokia to exercise its rights relating to the Feedback.

6. EXCLUSION OF WARRANTIES 

6.1 LICENSEE ACKNOWLEDGES THAT THE LICENSED SOFTWARE IS PROVIDED "AS IS" AND
NOKIA, ITS AFFILIATES AND/OR ITS LICENSORS DO NOT MAKE ANY REPRESENTATIONS OR
WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR THAT THE LICENSED
SOFTWARE WILL NOT INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR
OTHER RIGHTS. 

6.2 THERE IS NO WARRANTY BY NOKIA, ITS AFFILIATES AND/OR ITS LICENSORS OR BY
ANY OTHER PARTY THAT THE LICENSED SOFTWARE WILL MEET THE LICENSEE'S
REQUIREMENTS OR THAT THE OPERATION OF THE LICENSE SOFTWARE WILL BE
UNINTERRUPTED OR ERROR-FREE. LICENSEE ASSUMES ALL RESPONSIBILITY AND RISK FOR
THE SELECTION OF THE LICENSED SOFTWARE TO ACHIEVE LICENSEE'S INTENDED RESULTS
AND FOR THE INSTALLATION, USE, AND RESULTS OBTAINED FROM IT. 

6.3 THIS AGREEMENT CREATES NO OBLIGATIONS ON THE PART OF NOKIA OTHER THAN AS
SPECIFICALLY SET FORTH HEREIN. 

7. LIMITATION OF LIABILITY

7.1 IN NO EVENT SHALL NOKIA, ITS EMPLOYEES, LICENSORS, AFFILIATES OR AGENTS BE
LIABLE FOR ANY LOST PROFITS OR COSTS OF PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES, PROPERTY DAMAGE, LOSS OF PROFITS, INTERRUPTION OF BUSINESS OR FOR ANY
SPECIAL, INDIRECT, INCIDENTAL, ECONOMIC, COVER, PUNITIVE, OR CONSEQUENTIAL
DAMAGES, HOWEVER CAUSED, AND WHETHER ARISING UNDER CONTRACT, TORT, NEGLIGENCE,
OR OTHER THEORY OF LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE THE
LICENSED SOFTWARE, EVEN IF THE OTHER PARTY IS ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES. 

7.2 THE TOTAL AGGREGATE LIABILITY OF NOKIA SHALL, ON ALL OCCASIONS AND FOR ANY
DAMAGES OF WHATSOEVER NATURE ARISING IN WHATSOEVER MANNER, SHALL BE LIMITED TO
U.S. $10. 

8. TECHNICAL SUPPORT
 
8.1 In connection with the delivery of the Licensed Software under this
Agreement, Nokia may make technical support relating to the Licensed Software
available to the Licensee e.g. by providing access to a dedicated support
website, but Nokia has no obligation whatsoever to furnish the Licensee with
any technical support unless separately agreed in writing between Licensee and
Nokia.

8.2 In the event Nokia provides the Licensee with any technical support under
subsection 8.1 above without a separate written agreement, such provision shall
not (i) obligate Nokia to continue to provide any technical support, (ii)
enable the Licensee to make any claims regarding the quality, timeliness or
other characteristics of the technical support being provided, (iii) create any
obligation for Nokia to enter into a written support agreement with the
Licensee.

8.3 Any technical support that may be provided by Nokia shall be provided
without warranties of any kind, unless separately agreed in writing between the
Parties.

9. TERM AND TERMINATION

9.1 The Agreement shall become effective upon Acceptance by Licensee as set
forth above. Any terms and conditions that by their nature or otherwise
reasonably should survive a termination or an expiry of the Agreement shall be
deemed to survive. Such terms and conditions include but are not limited to the
obligations set forth in Clauses 1, 5 -8, 10-12.

9.2 Nokia will have the right to terminate the Agreement for any reason at any
time, in its sole discretion, with a prior written notice of thirty (30) days.

9.3 Upon expiry or termination of the Agreement Licensee shall return all
information received and materials made available by Nokia under this
Agreement, including but not limited to Licensed Software, to Nokia
immediately.

10. EXPORT CONTROL. 

The Licensed Software, including technical data, includes cryptographic
software subject to export controls under the U.S. Export Administration
Regulations ("EAR") prohibiting the use of the Licensed Software and technical
data by a Government End User, as defined hereafter, without a license from the
U.S. government and may be subject to import or export controls in other
countries. A Government End User is defined in Part 772 of the EAR as "any
foreign central, regional, or local government department, agency, or other
entity performing governmental functions; including governmental research
institutions, governmental corporations, or their separate business units (as
defined in part 772 of the EAR) which are engaged in the manufacture or
distribution of items or services controlled on the Wassenaar Munitions List,
and international governmental organizations. This term does not include:
utilities (telecommunications companies and Internet service providers; banks
and financial institutions; transportation; broadcast or entertainment;
educational organizations; civil health and medical organizations; retail or
wholesale firms; and manufacturing or industrial entities not engaged in the
manufacture or distribution of items or services controlled on the Wassenaar
Munitions List.)". Licensee agrees to strictly comply with all applicable
import and export regulations and acknowledge that Licensee has the
responsibility to obtain licenses to export, re-export, transfer, or import the
Licensed Software. Licensee further represent that Licensee is not a Government
End User as defined above, and Licensee will not transfer the Licensed Software
to any Government End User without a license.

11. NOTICES.

All notices and return of the Licensed Software and Documentation should be
delivered to:

Head of Legal Department
NOKIA CORPORATION
Devices
P.O. Box 100
FIN-00045 NOKIA GROUP
FINLAND

12. MISCELLANOUS

12.1 Licensee and Nokia are independent Parties. Nothing in this the Agreement
shall be construed to make either Party an agent, employee, franchisee, joint
venture or legal representative of the other Party. Neither Party will have nor
represent itself to have any authority to bind the other Party or act on its
behalf. Nothing in these General Conditions or the Agreement shall constitute
or imply any promise or intention by Nokia to enter into any other business
arrangement with Licensee.

12.2 If any provision of the Agreement or its Appendices is held invalid, all
other provisions shall remain valid unless such validity would frustrate the
purpose of the Agreement, this Agreement and its Appendices shall be enforced
to the full extent allowable under applicable law. 

12.3 Licensee shall bear all costs that it may incur in connection with the use
of the Licensed Software. 

12.4 This Agreement is governed by the laws of Finland excluding its choice of
law provisions. All disputes arising from or relating to the Agreement shall be
settled by a single arbitrator appointed by the Central Chamber of Commerce of
Finland. The arbitration procedure shall take place in Helsinki, Finland in the
English language.
'''

class ProductFile(object):
    '''A file to be installed/removed with the product'''

    def __init__(self, directory, name, contents, is_executable):
        '''Constructor
        directory = directory where to install the file
        filename = name of the file to install
        contents = string contents to write, with newlines included
        is_executable = if True, the executable bit will be set during
                        installation'''
        self.directory = directory
        self.name = name
        self.contents = contents
        self.is_executable = is_executable
        self.path = os.path.join(self.directory, self.name)

    def install(self):
        '''Installs the file to the host system.'''
        MSGR.say("Installing file %s" % (self.path,), MSGR.SayTypeTask)

        if not os.path.isdir(self.directory):
            os.mkdir(self.directory)

        fo = open(self.path, "w")
        try:
            fo.write(self.contents)
        finally:
            fo.close()

        if self.is_executable:
            os.chmod(self.path, 0755)

    def remove(self):
        '''Removes the file to the host system.'''
        MSGR.say("Removing file %s" % (self.path,), MSGR.SayTypeTask)
        os.remove(self.path)

# files to install/remove with the product
PRODUCT_FILES = []

def running_as_root():
    '''Returns True if script has root privileges, False otherwise.'''
    return os.geteuid() == 0

class FriendlyException(Exception):
    '''User friendly exception, friendly because the traceback is not
       shown to the user.

       Raised in subroutines/methods to indicate failure. For this type of
       exception the setup will normally print the message to stdout and log
       the traceback.'''
    pass

class AbstractMethodException(Exception):
    '''Exception raised if an abstract method is called.'''
    pass

class CLIProgressThread(threading.Thread):
    '''Command line progress indication thread. Shows timed progress on the
    command line (right now prints dot every interval seconds) while the main
    thread is waiting for task processes to complete.

    This class was inspired by the lack of repetitive timer functionality in
    threading.Timer'''

    def __init__(self, interval):
        '''Constructor.
        interval = time in seconds, that elapses between progress characters'''
        threading.Thread.__init__(self)

        self.interval = interval
        self.__terminated = threading.Event() # if set must terminate self
        self.__running = threading.Event() # i.e. not paused by end()

        self.start()

    is_running = property(lambda self: self.__running.isSet())

    def begin(self):
        '''Beging the progress indication'''
        self.__running.set()

    def end(self):
        '''Stop the progress indication'''
        self.__running.clear()

    def join(self):
        '''Can be called by threading._exit (at least with Python 2.6.5) even
        before the owner object's (global Messenger) __del__ is called. So
        overriden to avoid deadlock, since default implementation will wait
        forever for the thread to quit the run method.'''
        self.__terminate()

        # for some reason on Python 2.6.5 things are garbage collected already:
        # so the commented call gives AttributeError: "'NoneType' object has no
        # attribute 'Thread'"
        # threading.Thread.join(self)
        super(self.__class__, self).join()

    def __terminate(self):
        '''Make this thread exit the run method.'''
        # make sure not called the multiple times
        if self.__terminated.isSet():
            return

        self.__terminated.set()

        # in case end() was called, thread is waiting for running, so wake it
        # up
        if not self.__running.isSet():
            self.__running.set() 

    def run(self):
        '''Runs the progress indication thread, i.e. prints progress character
        each time interval in seconds passes. If paused by the end() method
        will wait till begin() is called.'''
        while not self.__terminated.isSet():

            # running, so print progress after timeout
            if self.__running.isSet():
                self.__terminated.wait(self.interval)

                # was terminated while waiting
                if self.__terminated.isSet():
                    break

                # make sure end() wasn't called while thread was waiting
                if self.__running.isSet():
                    sys.stdout.write(".")
                    sys.stdout.flush()

            # not running, so wait until running is set
            else:
                self.__running.wait() 

class Messenger(object):
    '''A communication channel between the installation routines and the UI
    classes. Additionally handles all logging.'''

    # types of say
    SayTypeNormal = "say_normal"
    SayTypeTask = "say_task"
    SayTypePhase  = "say_phase"

    # possible values of task/phase done numbers
    DoneNumNotUsed = 0 # numbering is not in use
    DoneNumStart   = 1 # numbering was started

    def __init__(self):
        '''Constructor'''

        # list of strings describing non-critical errors that have occurred
        # during execution, there can be multiple non-critical errors
        self.__errors = []

        # string describing one (and only one) fatal error that has occurred,
        # if fatal error occurs setup will give up
        self.__fatal = ''

        # last thing said
        self.__last_say_msg = ''
        self.__last_say_type = None

        self.__override_say = None
        self.__override_ask = None

        # number of tasks, if total number is set, then number of done/total
        # tasks will be prepended to the message passed to say method and done
        # will be incremented with each say call
        self.__tasks_total_num = 0
        self.__tasks_done_num = self.__class__.DoneNumNotUsed

        self.__tasks_increment_notify_func = None

        # progress indication that is used only with tasks in command line UI
        self.__cli_progress = CLIProgressThread(5.0) 

        script_name = os.path.basename(sys.argv[0]) # in case it is full path
        script_name_no_ext = os.path.splitext(script_name)[0]

        # log file name for root
        if running_as_root():
            log_filename = "%s.log" % (script_name_no_ext,)

        # log file name for regular user
        else:
            log_filename = "%s-%s.log" % (script_name_no_ext,
                                          get_default_username())

        self.__fn_log = os.path.join(DIR_TEMP, log_filename)

        self.__fo_log = None

        while True:
            try:
                self.__fo_log = open(self.__fn_log, 'w')
            except Exception, e:
                print("Could not open log file %s (%s)!" % (self.__fn_log, e))
                self.__fn_log = make_unique_filename(self.__fn_log)
                print("Will try to use %s as log file" % (self.__fn_log,))
            else:
                break

        self.log("Python version: %s" % repr(sys.version))
        self.log("Setup version: %s" % MY_VERSION)

    fn_log = property(lambda self: self.__fn_log)
    fo_log = property(lambda self: self.__fo_log)

    def __del__(self):
        '''Destructor'''
        if self.__fo_log:
            self.__fo_log.close()

        self.__cli_progress.join()

    def cli_progress_stop(self):
        '''Stops the progress indication if it is running. It will be running
        for tasks when command line UI is used.'''
        if self.__last_say_type == self.__class__.SayTypeTask:
            if self.__cli_progress.is_running:
                self.__cli_progress.end()
                sys.stdout.write("\n")

    def log(self, msg, prefix = "V:"):
        '''Writes a log message.
        prefix = will be written before message'''
        self.__fo_log.write("%s [%s]: %s\n" % 
                          (prefix, time.strftime("%H:%M:%S %d.%m.%Y"), msg))
        self.__fo_log.flush()

    def warn(self, msg):
        '''Writes a warning message.'''
        self.log(msg, "W:")

    def log_exc(self):
        '''Writes current exception information into the log file'''
        fmt = '-' * 5 + " %s " + '-' * 5

        self.log(fmt % "Begin logging exception")
        traceback.print_exc(file = self.__fo_log)
        self.log(fmt % "End logging exception")

    def say(self, msg, say_type = SayTypeNormal):
        '''Writes a message to the UI, by default to the command line.
        Of course it is still possible to use print to write text to stdout,
        but if text is to be written to either GUI and command line UI, then
        this method should be used'''
        
        self.cli_progress_stop()

        # add task numbering
        if say_type == self.__class__.SayTypeTask and \
                (self.__tasks_done_num != self.__class__.DoneNumNotUsed):
            msg = "%d/%d %s" % (self.__tasks_done_num, self.__tasks_total_num,
                                msg)

        if self.__override_say:
            self.__override_say(msg, say_type, self.__tasks_done_num,
                                self.__tasks_total_num)
        else:
            # in command line tasks get progress indication
            if say_type == self.__class__.SayTypeTask:
                sys.stdout.write(msg + " ")
                sys.stdout.flush()
                self.__cli_progress.begin()
            else:
                sys.stdout.write(msg + "\n")

        self.log(msg, "S: (%s)" % say_type)

        self.__last_say_msg = msg
        self.__last_say_type = say_type

        # increment tasks done number
        if say_type == self.__class__.SayTypeTask and \
                (self.__tasks_done_num != self.__class__.DoneNumNotUsed):
            self.__increment_task_done_num()

    def set_override_say(self, new_say):
        '''Overrides the default UI say method. GUI can use this in order
        to redirect messages to the GUI
        new_say = function that takes (msg, say_type, done_num, total_num).
                  The last two have meaning if say_type is phase or task.
                  If done_num is DoneNumNotUsed, then the numbers are not
                  set and they should not be used at all.'''
        self.__override_say = new_say
        
    def remove_override_say(self):
        '''Removes the override for UI say method'''
        assert self.__override_say, "Override is not set!"
        self.__override_say = None

    def ask(self, title, question, choices, default_choice):
        '''Asks a question in the UI, by default the command line.'''
        self.cli_progress_stop()

        self.log("A: '%s' '%s' %s %s" % (title, question, choices,
                                         default_choice))

        if self.__override_ask:
            answer = self.__override_ask(title, question, choices,
                                         default_choice)
        else:
            answer = ask(title, question, choices, default_choice)

        self.log("A: Answer = %s" % answer)
        return answer

    def set_override_ask(self, new_ask):
        '''Overrides the default UI ask method. GUI can use this in order
        to redirect questions to the GUI
        new_ask  = new ask function'''
        self.__override_ask = new_ask
        
    def remove_override_ask(self):
        '''Removes the override for UI ask method'''
        assert self.__override_ask, "Override is not set!"
        self.__override_ask = None

    def get_fatal(self):
        '''fatal getter'''
        return self.__fatal

    def set_fatal(self):
        '''fatal setter'''
        assert not self.__fatal, "Fatal is already set!"

        self.__fatal = self.__last_say_msg
        self.log("Set fatal to: %s" % self.__fatal)
        self.cli_progress_stop()

    def get_errors(self):
        '''Returns sequence object containing errors'''
        return self.__errors

    def add_error(self):
        '''Adds an error to the collection of errors'''
        err_msg = self.__last_say_msg
        self.__errors.append(err_msg)
        self.cli_progress_stop()
        self.say("Failed: %s" % err_msg)

    def set_tasks_increment_notify_func(self, func):
        '''Sets the notification function. This function will be called when
        a task done number has been incremented.
        func = function that takes tasks done number and tasks total number'''

        assert callable(func), \
            "Notification function %s is not callable" % (func)

        self.__tasks_increment_notify_func = func

    def remove_tasks_increment_notify_func(self):
        '''Removes the notification function'''
        assert self.__tasks_increment_notify_func, \
            "Notification function is not set!"

        self.__tasks_increment_notify_func = None

    def set_tasks_total_num(self, num):
        '''Enables writing task numbers with say
        num = total number of tasks, current number will be incremented with
              each say call'''
        self.__tasks_total_num = num
        self.__tasks_done_num = self.__class__.DoneNumStart

    def __increment_task_done_num(self):
        '''Increments number of done task until it reaches total, then task
        numbering is disabled.'''
        self.__tasks_done_num += 1

        # disable task numbering
        if self.__tasks_done_num > self.__tasks_total_num:
            self.__tasks_done_num = self.__class__.DoneNumNotUsed

        else:
            if self.__tasks_increment_notify_func:
                self.__tasks_increment_notify_func(self.__tasks_done_num,
                                                   self.__tasks_total_num)

class sc:
    '''Common strings'''
    fatal_title = "Execution was aborted by fatal error during stage:"
    errors_title = "Non-critical errors occurred during these stages:"
    warn_cleartext_creds = ("WARNING! It is a high security risk to have "
                            "credentials written in cleartext on unencrypted "
                            "storage media!")

    @staticmethod
    def user_fail_refer_admin(cmd_name):
        '''Returns text that refers user to use admin command in case user
        command fails'''
        txt = ("Please fix the issues above, or run the %s as root with the "
               "'%s' command." % (MY_NAME, cmd_name))
        return txt

    @staticmethod
    def set_kernel_var_info(var, val):
        '''Returns text on how to set kernel variables.
        var = kernel variable to set
        val = value var must be set to'''

        txt = ("You can set a supported value for the current boot session "
               "with 'sysctl %(var)s=%(val)s' as root. For a permanent "
               "solution you may add '%(var)s = %(val)s' to %(file)s and run "
               "'sysctl -p' as root" %
               {"file" : SYS_INFO.sysctl_conf_fn, "var" : var, "val" : val})

        return txt

    @staticmethod
    def contact_info_on_problems():
        '''Returns contact info text for problematic situations.'''
        msgr_exists = bool(MSGR)

        txt_view_logfile = ""
        txt_attach_logfile = ""

        if msgr_exists:
            txt_view_logfile = ("\n\nPlease view %s for more info.\n\n"  %
                                (MSGR.fn_log,))

            txt_attach_logfile = ("\n\nNOTE! In either case remember to attach "
                                  "the logfile:\n%s" % (MSGR.fn_log))

        txt = ("%sIf you think this is due to a bug please report it to:\n%s"
               "\n\nIf you have questions please contact maintainer:\n%s (%s)"
               "%s" % (txt_view_logfile, CONTACT_BUGS_URL, CONTACT_NAME,
                CONTACT_EMAIL, txt_attach_logfile))

        return txt

    @staticmethod
    def summary(s, status):
        '''Returns summary text after command has completed its execution
        s = string class of the command
        status = status of the command'''
        txt = ''
        name_noun_cap = s.name_noun.capitalize()

        if status == Command.StatusCompleted:
            txt = "%s was successfully %s." % (PRODUCT_NAME,
                                               s.name_verb_simple_past)

        elif status == Command.StatusAborted:
            txt = "%s was aborted by the user."  % (name_noun_cap,)

        elif status == Command.StatusErrors:
            txt = "%s was %s despite some non-critical errors." % \
                (PRODUCT_NAME, s.name_verb_simple_past)

        elif status == Command.StatusFatal:
            txt = "%s was aborted by a fatal error." % (name_noun_cap)

        else:
            txt = "ERROR: SUMMARY NOT AVAILABLE!!!"

        return txt

class si:
    '''Strings for installation'''

    name_noun = "installation"
    name_verb_simple_present = "install"
    name_verb_present_continuous = "installing"
    name_verb_simple_past = "installed"

    intro_title = "Welcome to the %s installation wizard" % PRODUCT_NAME

    # without paragraph characters (using newlines to split paragraphs in GUI
    # makes the wizard too wide for some reason)
    intro_subtitle = (
        "%(par_begin)sThis application will guide you through the steps of "
        "getting %(product_name)s installed on your machine. The installation "
        "will take about %(cons_time)s and %(const_space)s of your disk "
        "space.%(par_end)s"
        "%(par_begin)sUpon a successful installation, you can find all the "
        "%(product_name_short)s content under %(sb_path)s.%(par_end)s" %
        {"product_name" : PRODUCT_NAME, "cons_time" : INST_CONS_TIME,
         "const_space" : INST_CONS_SPACE,
         "product_name_short" : PRODUCT_NAME_SHORT, "sb_path" : SB_PATH,
         "par_begin" : "%(par_begin)s", "par_end" : "%(par_end)s" })

    @staticmethod
    def intro_creds_warn():
        '''PKG_MGR is needed, hence this is a function'''
        txt = ("READ THIS CAREFULLY: Since %s is going to be installed from "
               "HTTPS repository, during installation your credentials "
               "will be stored in cleartext into %s and %s package will be "
               "installed into your system. This is all done to enable "
               "access to the HTTPS repository. %s" % 
               (SB_NAME, PKG_MGR.src_file, PKG_MGR.pkg_name_https_transport,
                sc.warn_cleartext_creds))
        return txt

    intro_subtitle_gui = intro_subtitle % {"par_begin" : "<p>", "par_end" : "</p>"}
    intro_subtitle_cli = intro_subtitle % {"par_begin" : "\n", "par_end" : "\n"}

    vdso_title = "VDSO support"
    vdso_subtitle = ("Current VDSO settings of this host are not "
                     "supported by %s." % (SB_NAME))
    vdso_info = (
        "To enable the installation of %(sb_name)s and %(product_name)s the "
        "%(my_name)s will set VDSO kernel parameter to %(sb_name)s supported "
        "value, which is 0. By default VDSO parameter will be set for this "
        "session only, which means the setting will persist until the next "
        "boot. If you would like the %(my_name)s to set VDSO parameter "
        "permanently " %
        {"sb_name" : SB_NAME, "product_name" : PRODUCT_NAME,
         "my_name" : MY_NAME})

    vdso_info_gui = vdso_info + "please check the checkbox below."
    vdso_info_cli = vdso_info + \
        "please answer yes ('%s') to the following question." % ASK_CHOICE_YES

    vdso_perm_text = "Permanently set VDSO kernel parameter to 0"
    # text with accelerators for GUI
    vdso_perm_text_acc = vdso_perm_text.replace("Permanently", "&Permanently")

    vdso_perm_hint = ("If checked the VDSO kernel parameter will be "
                      "permanently set to %s compatible value" % (SB_NAME))

    selinux_title = "SELinux support"
    selinux_subtitle = ("%s cannot be used while Security-Enhanced Linux "
                        "(SELinux) is in Enforcing mode." % (SB_NAME))
    selinux_info = (
        "To enable the installation of %(sb_name)s and %(product_name)s the "
        "%(my_name)s will set SELinux to Permissive mode during the "
        "installation. If you would like the %(my_name)s to set SELinux to "
        "Permissive mode permanently " %
        {"sb_name" : SB_NAME, "product_name" : PRODUCT_NAME,
         "my_name" : MY_NAME})

    selinux_info_gui = selinux_info + "please check the checkbox below."
    selinux_info_cli = selinux_info + \
        "please answer yes ('%s') to the following question." % ASK_CHOICE_YES

    selinux_perm_text = "Permanently set SELinux to Permissive mode"
    # text with accelerators for GUI
    selinux_perm_text_acc = \
        selinux_perm_text.replace("Permanently", "&Permanently")

    selinux_perm_hint = ("If checked SELinux will be permanently set to %s "
                         "compatible (Permissive) mode" % (SB_NAME))

    mmap_mina_title = "mmap_min_addr support"
    mmap_mina_subtitle = ("Host kernel mmap_min_addr value is incompatible "
                          "with the %s version used in %s." %
                          (EMU_NAME, SB_NAME))
    mmap_mina_info = (
        "For %(emu_name)s to be able to run, the %(my_name)s will set "
        "mmap_min_addr kernel parameter to %(emu_name)s supported "
        "value. By default mmap_min_addr parameter will be set for this "
        "session only, which means the setting will persist until the next "
        "boot. If you would like the %(my_name)s to set mmap_min_addr "
        "parameter permanently " %
        {"emu_name" : EMU_NAME, "my_name" : MY_NAME})

    mmap_mina_info_gui = mmap_mina_info + "please check the checkbox below."
    mmap_mina_info_cli = mmap_mina_info + \
        "please answer yes ('%s') to the following question." % ASK_CHOICE_YES

    mmap_mina_perm_text = "Permanently set mmap_min_addr kernel parameter"
    # text with accelerators for GUI
    mmap_mina_perm_text_acc = mmap_mina_perm_text.replace("Permanently",
                                                          "&Permanently")

    mmap_mina_perm_hint = ("If checked the mmap_min_addr kernel parameter "
                           "will be permanently set to %s compatible value" %
                           (EMU_NAME))

    users_title = "Users"
    users_subtitle = ("Please select users to install %s targets for" % (PRODUCT_NAME_SHORT))
    users_list_hint = ("List of users on the system. %s will be "
                       "installed for selected user." % PRODUCT_NAME)

    targets_title = "Targets"

    @staticmethod
    def targets_subtitle(usernames):
        '''Targets subtitle must be passed list of names of users that have
        the targets already'''
        usernames_str = seq_to_str(usernames, ", ", "")

        if len(usernames) > 1:
            u = "users"
        else:
            u = "user"

        txt = ("The targets %s or %s already exist for the %s %s. Please "
               "choose appropriate action." %
               (TARGET_X86.name, TARGET_ARMEL.name, u, usernames_str))
        return txt

    targets_txt_overwrite = "Overwrite existing targets"
    targets_txt_create = "Create new targets"

    # text with accelerators for GUI
    targets_txt_overwrite_acc = \
        targets_txt_overwrite.replace("Overwrite", "&Overwrite")
    targets_txt_create_acc = targets_txt_create.replace("new", "n&ew")

    targets_create_title = "To create new targets using different name prefix:"
    targets_create_title_acc = \
        targets_create_title.replace("create", "&create")
    targets_create_subtitle = (
        "Specify a prefix to be used for the new targets. Ensure that it is "
        "not the same as any of the existing %s targets (eg: %s). When the "
        "new targets are created, the architecture (X86 or ARMEL) will be "
        "automatically added to the prefix." % (SB_NAME, Target.name_prefix))

    targets_exists_title = "Prefix exists!"
    # must be formatted with target prefix names and list of usernames that
    # already use that target name
    targets_exists_txt = (
        "A " + SB_NAME + " target with the prefix %s already exists for "
        "the user(s) %s. Please specify a different prefix.")

    license_title = "End User Software Agreement"
    license_subtitle = ("To proceed, please read and accept the terms of the "
                        "license agreement")

    summary_title = "Summary"
    summary_subtitle = ("Please review installation configuration. At this "
                        "point, you can still go back and change settings")
    
    summary_txt_users = "Users to install %s for:" % (PRODUCT_NAME_SHORT,)
    summary_txt_vdso = "Permanently set VDSO:"
    summary_txt_selinux = selinux_perm_text + ":"
    summary_txt_mmap_mina = "Permanently set mmap_min_addr:"
    summary_txt_eusa = "Install Nokia Binaries:"
    summary_txt_targets_prefix = "Target name prefix"

    progress_title = "Installing"
    progress_subtitle = "Please wait, installation in progress..."
    progress_subtitle_done = "Done installing"

    conclusion_title = "Installation of %s completed" % PRODUCT_NAME
    conclusion_subtitle = "Thank you for using %s!" %PRODUCT_NAME

class sr:
    '''Strings for removal'''

    name_noun = "removal"
    name_verb_simple_present = "remove"
    name_verb_present_continuous = "removing"
    name_verb_simple_past = "removed"

    intro_title = "Welcome to the %s removal wizard" % PRODUCT_NAME
    intro_subtitle = ("This application will help you remove the %s "
"and the %s from your system. As a result, about %s of your disk space "
"will be freed." % (PRODUCT_NAME, SB_NAME, INST_CONS_SPACE))

    intro_subtitle_gui = intro_subtitle
    intro_subtitle_cli = intro_subtitle

    summary_title = "Summary"
    summary_subtitle = ("Please review removal configuration")
    summary_txt_scratchbox = "Remove the %s:" % (SB_NAME,)
    summary_txt_sb_homes = \
        "Remove all user home directories in %s:" % (SB_NAME,)
    summary_txt_targets = "Remove all %s targets:" % (SB_NAME,)
    summary_txt_files = "Remove all %s files:" % (PRODUCT_NAME,)
    summary_txt_verify = ("Are you sure? Those files and directories will be "
                          "completely removed!")

    progress_title = "Removing"
    progress_subtitle = "Please wait while removing..."
    progress_subtitle_done = "Done removing"

    conclusion_title = "Removal of %s completed" % PRODUCT_NAME
    conclusion_subtitle = "Thank you for using %s!" % PRODUCT_NAME

class CmdTask(object):
    '''A command task'''

    # nop, is a name of task that does nothing. It is added to the task queue
    # if some tasks in it say more than once. This is done to make the length
    # of queue to be equal to the total number of say calls.
    nop = 'nop'

    def __init__(self, name, func, *args, **kwds):
        '''Constructor.
        name = Name that will be said before running the task. If set to None,
               won't be said. If name is set to nop then task is nop, in which
               case other arguments are not used at all.
        func = function that executes the task
        args = Arguments to pass to func
        kwds = Keyword arguments to pass to func'''

        # if name is not nop, func must be callable
        assert callable(func) or name == self.__class__.nop, \
            "Task function %s is not callable" % (func)

        self.__name = name
        self.__func = func
        self.__args = args
        self.__kwds = kwds

    def run(self):
        '''Runs a task'''

        # nop task does nothing
        if self.__name == self.__class__.nop:
            return

        if self.__name:
            MSGR.say(self.__name, MSGR.SayTypeTask)

        MSGR.log("Starting task function")
        self.__func(*self.__args, **self.__kwds)

    def __str__(self):
        '''String representation method.'''
        return ("%s, %s, %s, %s" %
                (self.__name, self.__func, cut_strs_in_container(self.__args),
                 cut_strs_in_container(self.__kwds)))

class CmdTasksQ(object):
    '''A queue of command tasks.'''

    def __init__(self):
        '''Constructor.'''
        self.__tasks = []
        
    def append_nop(self, num = 1):
        '''Appends nop tasks to the queue.
        num = number of nops to append'''

        assert num >= 1, "Number must be more than 0!"

        for i in xrange(num):
            self.append_named(CmdTask.nop, CmdTask.nop)

    def append(self, func, *args, **kwds):
        '''Appends a task to the queue.
        See task class constructor for description of the arguments.'''
        self.append_named(None, func, *args, **kwds)

    def append_named(self, name, func, *args, **kwds):
        '''Appends a named task to the queue.
        See task class constructor for description of the arguments.'''
        self.__tasks.append(CmdTask(name, func, *args, **kwds))

    def run_all(self, should_abort = None):
        '''Runs all queued tasks

        If UI is under testing, there is no point in entering the run method of
        tasks since unnamed task say in their functions anyways, so say
        messages cannot be retrieved without running the task functions, unless
        the logic is moved to the tasks functions...'''

        # print contents of tasks
        tasks_str = seq_to_str(
            ["%s %s" % (i + 1, o) for i, o  in enumerate(self.__tasks)])
        MSGR.log("Running command tasks:\n%s" % tasks_str)

        if TESTING_UI:
            MSGR.log("Not running task because of UI testing")
            return

        for task in self.__tasks:

            MSGR.log("Processing task %s" % task)

            if should_abort and should_abort(): 
                MSGR.log("Aborting...")
                break

            try:
                task.run()
            except CmdErrorFailure, e: # non-critical error
                MSGR.log_exc()
                continue

    def __len__(self):
        '''Returns the length of the tasks queue'''
        return len(self.__tasks)

class CmdPhase(object):
    '''A command is split into phases. Phases are split into tasks.

    Some phases are dynamic, meaning they decide tasks on the fly. Normally,
    dynamic phases require the previous phase to be completed before they can
    decide their tasks.'''

    def __init__(self, name, get_tasks_func):
        '''Constructor.
        name = name of the phase, will be said when phase is started
        get_tasks_func = callable that takes the same arguments as the run
                         method of Command class. Most likely call to this
                         function won't succeed before preceding phase has
                         been completed.'''

        assert callable(get_tasks_func), \
            "Get task function %s is not callable" % (get_tasks_func)

        self.__name = name
        self.__get_tasks = get_tasks_func

    def run(self, say_msg_prepend, users_selections, should_abort = None):
        '''Runs a phase. Takes same arguments as the run method of Command.'''
        MSGR.say(say_msg_prepend + self.__name, Messenger.SayTypePhase)
        tasks = self.__get_tasks(users_selections)
        MSGR.set_tasks_total_num(len(tasks))
        tasks.run_all(should_abort)
        # for command line only, to separate phases (GUI will replace it fast)
        MSGR.say("")

def cfail(msg):
    '''For check functions, if check fails, they call this with error
    message'''
    raise FriendlyException(msg)

def check_running_as_root():
    '''Checks if the script is run by the root.'''
    if not running_as_root():
        cfail("This command should be run as root!")

def check_running_as_non_root():
    '''Checks if the script is run by a regular user.'''
    if running_as_root():
        cfail("This command should not be run as root!")

def check_running_out_of_sb():
    '''Checks that script is not running inside scratchbox.'''
    if os.path.exists("/targets/links/scratchbox.config"):
        cfail("This script needs to be run outside of %s." % (SB_NAME,))

def check_sb_installed():
    '''Checks if scratchbox is installed.'''
    if not SYS_INFO.has_scratchbox:
        cfail(
            "%(sb_name)s not found in installation path '%(sb_path)s'. "
            "Please complete %(sb_name)s installation first." %
            {"sb_name" : SB_NAME, "sb_path" : SB_PATH})

def check_sb_user_exists(username):
    '''Checks if user exists in scratchbox.'''
    path = os.path.join(SB_PATH, "users", username)

    if not os.path.isdir(path):
        cfail(
            "%(sb_name)s directory for user not present. Add user with\n"
            "'%(sb_path)s/sbin/sbox_adduser %(username)s'." %
            {"sb_name" : SB_NAME, "sb_path" : SB_PATH,
             "username" : username})

def check_sb_conf_exists():
    '''Checks if sb-conf exists.'''
    path = os.path.join(SB_PATH, "tools/bin/sb-conf")

    if not os.path.exists(path):
        cfail(
            "%(sb_name)s sb-conf tool not found in '%(sb_path)s'. This is "
            "most likely due to old version of %(sb_name)s. Please "
            "complete %(sb_name)s installation first." %
            {"sb_name" : SB_NAME, "sb_path" : SB_PATH})

def check_sb_bind_mount(username):
    '''Check for scratchbox bind mount.'''
    path = os.path.join(SB_PATH, "users", username, "scratchbox")

    if not os.path.isdir(path):
        cfail(
            "%(sb_name)s bind mount for user not present. Start "
            "%(sb_name)s service with "
            "'sudo %(sb_path)s/sbin/sbox_ctl start'." %
            {"sb_name" : SB_NAME, "sb_path" : SB_PATH})

def check_sb_dev_null(username):
    '''Check if scratchbox properly set up (/dev/null is readable)'''
    path = os.path.join(SB_PATH, "users", username, "dev/null")

    if not os.access(path, os.R_OK):
        cfail(
            "%(sb_name)s user's /dev is not properly set up. Couldn't "
            "read /dev/null. Start %(sb_name)s service with\n"
            "'sudo %(sb_path)s/sbin/sbox_ctl start'." %
            {"sb_name" : SB_NAME, "sb_path" : SB_PATH})

def check_sb_user_home(username):
    '''Checks for users home directory inside scratchbox.'''
    path = os.path.join(SB_PATH, "users", username, "home", username)

    if not os.path.isdir(path):
        cfail(
            "%(sb_name)s home directory '%(sb_home)s' not found. "
            "Add user with "
            "'%(sb_path)s/sbin/sbox_adduser %(username)s'." %
            {"sb_name" : SB_NAME, "sb_home" : path,
             "sb_path" : SB_PATH, "username" : username})

def check_sb_login():
    '''Checks if scratchbox login is readable and executable.
    TODO: check login for each user separately?'''
    path = os.path.join(SB_PATH, "login")

    # check if scratchbox login is readable
    if not os.access(path, os.R_OK):
        cfail(
            "%(sb_name)s login not found in '%(sb_path)s'. "
            "Please complete %(sb_name)s installation first." %
            {"sb_name" : SB_NAME, "sb_path" : SB_PATH})

    # check if scratchbox login is executable (user in sbox group)
    if not os.access(path, os.X_OK):
        cfail(
            "%(sb_name)s login found but not executable by user. Please "
            "check that user is member of the group specified in "
            "%(sb_name)s installation (default '%(sb_group)s'). Also "
            "start a new login terminal after adding group membership." % 
            {"sb_name" : SB_NAME, "sb_group" : SB_GROUP})

def check_sb_version(sb_version_min_str):
    '''Checks if scratchbox version is supported.
    sb_version_min_str = string with the the oldest scratchbox version that
                         is supported'''

    sb_version_ins_str = sb_get_version()

    if not sb_version_ins_str:
        cfail(
            "Couldn't execute %(sb_name)s utility sb-conf to get "
            "%(sb_name)s version. Please complete %(sb_name)s "
            "installation first." % {"sb_name" : SB_NAME})

    # check for scratchbox version
    # use "dpkg --compare-versions" on Debian systems???
    sb_version_ins_list = version_str_to_list(sb_version_ins_str, SB_NAME)
    sb_version_min_list = version_str_to_list(sb_version_min_str, SB_NAME)

    if sb_version_ins_list < sb_version_min_list:
        cfail(
            "%(sb_name)s version is too old (scratchbox-core %(version_ins)s)."
            "The minimum required scratchbox-core version is %(version_min)s. "
            "Please refer to http://scratchbox.org/" %
            {"sb_name" : SB_NAME, "version_ins" : sb_version_ins_str,
             "version_min" : sb_version_min_str})

def check_sb_cputransp():
    '''Checks for CPU transparency method.'''
    if not sb_has_cputransp(TARGET_ARMEL.cputransp):
        cfail(
            "CPU transparency method '%s' not found. Please complete %s "
            "installation first." % (TARGET_ARMEL.cputransp, SB_NAME))

def check_sb_toolchains():
    '''Checks that toolchains are found.'''
    if (not sb_has_toolchain(TARGET_ARMEL.toolchain) or
        not sb_has_toolchain(TARGET_X86.toolchain)):
        cfail(
            "Toolchain %s required for '%s' target. Toolchain %s required "
            "for '%s' target. Please complete %s installation first." %
            (TARGET_ARMEL.toolchain, TARGET_ARMEL.name, TARGET_X86.toolchain,
             TARGET_X86.name, SB_NAME))

def check_sb_devkits():
    '''Checks that devkits are found.'''
    missing_devkits = sb_get_missing_devkits()

    if missing_devkits:
        cfail("%(sb_name)s devkits %(devkits)s not found. Please complete "
              "%(sb_name)s installation first." % 
              {"sb_name" : SB_NAME,
               "devkits" : seq_to_str(missing_devkits, ", ", "")})

def check_sb_sessions():
    '''Checks for scratchbox sessions running for the default user.'''
    if sb_has_sessions():
        cfail("You must close your other %(sb_name)s sessions first." %
              {"sb_name" : SB_NAME})

def check_sb_sessions_all_users():
    '''Checks if any of the scratchbox users have open sessions.'''
    session_usernames = sb_get_sessions_usernames()

    if session_usernames:
        cfail("The following users have %s sessions open: %s. These "
              "sessions must be closed first." %
              (SB_NAME, seq_to_str(session_usernames, ", ", "")))

def check_vdso():
    '''Checks if vdso is supported'''
    if SYS_INFO.has_unsup_vdso:
        cfail(
            si.vdso_subtitle + " " + 
            sc.set_kernel_var_info(SYS_INFO.vdso_var, SYS_INFO.vdso_val))

def check_selinux():
    '''Checks if SELinux is supported.'''
    if SYS_INFO.has_unsup_selinux:
        cfail(si.selinux_subtitle +
              " You can change to Permissive mode with\n'setenforce 0'")

def check_mmap_min_addr():
    '''Checks if mmap_min_addr is supported.'''
    if SYS_INFO.has_unsup_mmap_mina:
        cfail(
            si.mmap_mina_subtitle + " " +
            sc.set_kernel_var_info(SYS_INFO.mmap_mina_var,
                                   SYS_INFO.mmap_mina_val))

def check_running_out_of_fakeroot():
    '''Checks that script is run out of the fakeroot environment'''
    if os.environ.has_key('FAKEROOTKEY'):
        cfail("This script should not be run inside fakeroot.")

def check_binfmt_misc():
    '''Checks that kernel has the binfmt_misc module.'''
    if not os.path.isdir("/proc/sys/fs/binfmt_misc"):
        cfail("Host kernel module binfmt_misc is required for the CPU "
              "transparency feature.")

def check_ipv4_port_range():
    '''Checks host kernel local IPv4 port range. This check is from the legacy
    Scratchbox installer, no one knows if it is really needed.'''
    p = subprocess_Popen(
        "cat /proc/sys/net/ipv4/ip_local_port_range | awk '{ print ($2-$1) }'",
        stdout = subprocess.PIPE, stderr = subprocess.STDOUT, shell = True)

    ipv4_range = int(p.communicate()[0].strip())

    if ipv4_range < 10000:
        cfail("Host kernel has IPv4 local port range under 10000. This "
              "causes problems with fakeroot. Increase with "
              "'echo \"1024 65000\" > /proc/sys/net/ipv4/ip_local_port_range'")

def check_sb_path_sane():
    '''Checks that Scratchbox installation path is sane. Legacy Scratchbox
    installer checks this only if installing from tarballs. This one checks
    always - good assertion.'''
    if not os.path.isabs(SB_PATH):
        cfail("%s install path must start with '/'." % (SB_NAME,))

    end = "/scratchbox"
    if not SB_PATH.endswith(end):
        cfail("%s install path must end with '%s'." % (SB_NAME, end,))

def check_sb_path_exists():
    '''Checks that Scratchbox installation path exists.'''
    if not os.path.isdir(SB_PATH):
        cfail("%s is not installed: path '%s' does not exist." %
              (SB_NAME, SB_PATH))

def check_binfmt_misc_arm():
    '''Checks binfmt_misc contents for confilicting arm binaries.'''

    arm_magic = "7f454c4600010000000000000000000000002800"
    binfmt_dir = "/proc/sys/fs/binfmt_misc/"

    for binfmt in os.listdir(binfmt_dir):
        binfmt_path = os.path.join(binfmt_dir, binfmt)

        if binfmt not in ("status", "register", "sbox-arm"):
            # arm magic found
            if not exec_cmd('grep -q "magic %s" %s' % (arm_magic, binfmt_path)):

                txt = subprocess_Popen('grep "^interpreter " %s' % (binfmt_path),
                                       stdout = subprocess.PIPE,
                                       stderr = subprocess.STDOUT,
                                       shell = True
                                       ).communicate()[0].strip()

                cfail("Conflicting registration for arm programs with "
                      "binfmt_misc. Please remove the conflicting package or "
                      "program, and try again. Conflicting\n%s" % (txt))

class Command(object):
    '''A command.
    A command is split into phases, which in their turn consist of tasks.'''

    # execution status of a command
    StatusNotStarted = "status_not_started"
    StatusRunning    = "status_running"
    StatusCompleted  = "status_completed" # successful completion
    StatusAborted    = "status_aborted"   # aborted by the user
    StatusErrors     = "status_errors"    # non-critical errors occured
    StatusFatal      = "status_fatal"     # fatal error stopped the command execution

    # children should override these as needed
    name = "command" # name of the command as specified by the user
    aliases = ["c"]    # possible name aliases/alternatives

    is_install = False # command installs product
    is_remove = False # command removes product
    needs_root_access = False # command must be run by root

    s = None # UI strings class
    desc = "" # short description for the command line UI usage message

    def __init__(self):
        '''Constructor.'''
        self.is_running_last_task = False
        self.is_running_last_phase = False

        self.status = self.__class__.StatusNotStarted

        self.run_checks()

    def get_phases_list(self):
        '''Returns list of phases. Should be overridden in children.'''
        pass

    def get_default_checks_list(self):
        '''Returns list of common checks that are the same for all Command
        classes.'''
        checks_list = []

        if self.needs_root_access: # first check for root access
            checks_list.append((check_running_as_root,))
        else:
            checks_list.append((check_running_as_non_root,))

        checks_list.append((check_running_out_of_sb,))
        checks_list.append((check_sb_path_sane,))

        return checks_list

    def get_checks_list(self):
        '''Returns a tuple containing list of of checks and a string. Checks is
        a list of tuples, where each element is a function and possible
        arguments. String is the error message to be appended to the message
        from check, if it is empty it will not be used. This method should be
        overridden in children.'''
        return ([], "")

    def run_checks(self):
        '''Runs the checks'''
        (checks_list, check_fail_info_msg) = self.get_checks_list()
        checks_list = self.get_default_checks_list() + checks_list

        MSGR.log("About to run %s command checks %s" % (self.name, checks_list))

        assert len(checks_list) == len(set(checks_list)), \
            "There are duplicate checks!"

        for check in checks_list:
            MSGR.log("Running check %s" % (check,))

            check_func = check[0]
            
            try:
                if len(check) > 1:
                    check_func(*check[1:])
                else:
                    check_func()

            except FriendlyException, e: # check failed
                MSGR.log("Check failed")

                # add message to text if available
                if check_fail_info_msg:
                    raise FriendlyException(
                        "%s\n\n%s" % (str(e), check_fail_info_msg))
                else:
                    raise

    def run(self, users_selections, should_abort = None):
        '''Runs the command (gets list of phases and runs them).
        users_selections = run command with these selections
        should_abort = function that returns boolean value, if that value
                       is true then command will be aborted'''

        MSGR.log("Running command with selections: %s" % (users_selections))
        self.status = self.__class__.StatusRunning

        phases_list = self.get_phases_list()

        MSGR.set_tasks_increment_notify_func(self.on_tasks_increment)

        try:

            for index, phase in enumerate(phases_list):

                if phase == phases_list[-1]:
                        self.is_running_last_phase = True
                        MSGR.log("Started phase is the last one")

                if len(phases_list) == 1:
                    say_msg_prepend = ""
                else:
                    say_msg_prepend = ("Phase %s of %s: " %
                                       (index + 1, len(phases_list)))

                phase.run(say_msg_prepend, users_selections, should_abort)

            # if there is fatal this place isn't reached
            if MSGR.get_errors():
                self.status = self.__class__.StatusErrors
            else:
                self.status = self.__class__.StatusCompleted

            if should_abort and should_abort():
                self.status = self.__class__.StatusAborted

        # catch even unpredictable exceptions
        except Exception, e: # (CmdFatalFailure, FriendlyException), e:
            MSGR.set_fatal()
            MSGR.log(str(e))
            MSGR.log_exc()
            self.status = self.__class__.StatusFatal

        MSGR.remove_tasks_increment_notify_func()

    def on_tasks_increment(self, tasks_done_num, tasks_total_num):
        '''Called by Messenger when task done number is incremented.'''
        if self.is_running_last_phase:
            if tasks_done_num == tasks_total_num:
                MSGR.log("Started task is the last one")
                self.is_running_last_task = True

class AdminInstallCommand(Command):
    '''Admin install command.'''

    name = "admininstall"
    aliases = ["ai"]

    is_install = True
    needs_root_access = True

    s = si
    desc = ("Install the %s and the %s as root for any user" %
            (SB_NAME, PRODUCT_NAME))

    def __init__(self):
        '''Constructor.'''
        Command.__init__(self)

    def get_phases_list(self):
        '''See parent's description of this method'''
        phase1 = CmdPhase("Installing the %s" % (PRODUCT_NAME,),
                          self.get_phase1_tasks)

        return [phase1]

    def get_phase1_tasks(self, install_selections):
        '''Returns tasks for phase 1'''
        tasks = CmdTasksQ()

        if SYS_INFO.has_unsup_vdso:
            tasks.append(set_kernel_param,
                         SYS_INFO.vdso_var,
                         SYS_INFO.vdso_val,
                         install_selections.set_perm_vdso)

        if SYS_INFO.has_unsup_selinux:
            tasks.append(set_selinux_permissive,
                         install_selections.set_perm_selinux)

        if SYS_INFO.has_unsup_mmap_mina:
            tasks.append(set_kernel_param,
                         SYS_INFO.mmap_mina_var,
                         SYS_INFO.mmap_mina_val,
                         install_selections.set_perm_mmap_mina)

        # install needed extra packages
        pkgs_to_install = [PKG_MGR.pkg_name_xephyr]

        if PKG_MGR.repo_requires_authentication():
            pkgs_to_install.append(PKG_MGR.pkg_name_https_transport)

        for pkg in pkgs_to_install:
            if not PKG_MGR.pkg_is_installed(pkg):
                tasks.append(PKG_MGR.pkg_install, pkg, False)

        tq_append_install_sb_tasks(tasks, install_selections)
        
        num_users = len(install_selections.usernames)
        
        if num_users: # instlal SDK only if users selected
            tq_append_install_sdk_tasks(tasks, install_selections)
            
        # install files
        for f in PRODUCT_FILES:
            tasks.append(f.install)

        return tasks

    def get_checks_list(self):
        '''See parent's description of this method'''
        checks_list = [(check_running_out_of_fakeroot,),
                       (check_binfmt_misc,),
                       (check_ipv4_port_range,),
                       (check_binfmt_misc_arm,),
                       ]

        if SYS_INFO.has_scratchbox:
            checks_list.append((check_sb_sessions_all_users,))

        return (checks_list, "")

class UserInstallCommand(Command):
    '''User envrionment installation command.'''

    name = "userinstall"
    aliases = ["ui"]

    is_install = True

    s = si
    desc = "Install the %s for the user running the setup" % (PRODUCT_NAME,)

    # the oldest scratchbox version that this command supports
    sb_version_min_str = "1.0.18"

    def __init__(self):
        '''Constructor.'''
        Command.__init__(self)

    def get_phases_list(self):
        '''See parent's description of this method'''
        phase1 = CmdPhase("Installing the %s" % PRODUCT_NAME,
                          self.get_phase1_tasks)

        return [phase1]

    def get_phase1_tasks(self, install_selections):
        '''Returns tasks for phase 1'''
        tasks = CmdTasksQ()

        tq_append_install_sdk_tasks(tasks, install_selections)

        return tasks

    def get_checks_list(self):
        '''See parent's description of this method'''

        username = get_default_username()

        checks_list = [(check_sb_installed,),
                       (check_sb_user_exists, username),
                       (check_sb_conf_exists,),
                       (check_sb_bind_mount, username),
                       (check_sb_dev_null, username),
                       (check_sb_user_home, username),
                       (check_sb_login,),
                       (check_sb_version, self.sb_version_min_str),
                       (check_sb_cputransp,),
                       (check_sb_toolchains,),
                       (check_sb_devkits,),
                       (check_sb_sessions,),
                       (check_vdso,),
                       (check_selinux,),
                       (check_mmap_min_addr,),
                       ]

        check_fail_info_msg = \
            sc.user_fail_refer_admin(AdminInstallCommand.name)

        return (checks_list, check_fail_info_msg)

class AdminRemoveCommand(Command):
    '''Admin remove command.'''

    name = "adminremove"
    aliases = ["ar"]

    is_remove = True
    needs_root_access = True

    s = sr
    desc = "Remove the %s and the %s as root" % (SB_NAME, PRODUCT_NAME,)

    def __init__(self):
        '''Constructor.'''
        Command.__init__(self)

    def get_phases_list(self):
        '''See parent's description of this method'''
        phase1 = CmdPhase("Removing the %s" % (PRODUCT_NAME,),
                          self.get_phase1_tasks)

        return [phase1]

    def get_phase1_tasks(self, remove_selections):
        '''Returns tasks for phase 1'''
        tasks = CmdTasksQ()

        # Debian system
        if isinstance(PKG_MGR, AptPkgManager):
            # pkg manager does not purge, hence:
            tasks.append_named(
                "Removing %s packages" % (SB_NAME), exec_cmd_fatal,
                "apt-get remove scratchbox-\* -y --purge")
            tasks.append(PKG_MGR.repo_remove)

        # non-Debian system
        else:
            tasks.append_named("Stopping %s" % (SB_NAME,), exec_cmd_fatal,
                               "%s/sbin/sbox_ctl stop" % (SB_PATH,))

        tasks.append(sb_run_prermdir_sanity_checks)

        # remove Scratchbox directory
        tasks.append(remove_dir_tree, SB_PATH)

        # remove files
        for f in PRODUCT_FILES:
            if os.path.exists(f.path):
                tasks.append(f.remove)

        return tasks

    def get_checks_list(self):
        '''See parent's description of this method'''
        checks_list = [(check_sb_path_exists,),
                       (check_running_out_of_fakeroot,),
                       (check_sb_sessions_all_users,)
                       ]

        return (checks_list, "")

def cmd_name2class(name, cmd_classes):
    '''Converts a command name or alias into a respective command class.
    name = name or alias of a command
    cmd_classes = list of command classes to search for name'''
    name = name.lower()

    for cmd_class in cmd_classes:
        # valid name or alias
        if name == cmd_class.name or name in cmd_class.aliases:
            return cmd_class

    return None

def create_command(args):
    '''Parses the command arguments and creates a command object based on the
    arguments. If command is not properly specified will ask the user for the
    command. Returns command object.
    args = command line arguments (the specified command)'''
    sup_cmd_classes = create_command.sup_cmd_classes
    sorted_cmd_names = sorted([i.name for i in sup_cmd_classes])
    num_args = len(args)
    cmd_class = None # the class of the command selected by the user
    lcmd_name2class = lambda name: cmd_name2class(name, sup_cmd_classes)
    cmd_class_from_menu = lambda title: \
        lcmd_name2class(cli_menu(title, sorted_cmd_names))

    # command not specified, so ask
    if num_args == 0:
        title = ("You forgot to specify a command. Please choose one from "
                 "available commands.")
        cmd_class = cmd_class_from_menu(title)

    # too many commands specified, choose the first correct one
    elif num_args > 1:
        print "More than one command specified, first valid one will be used"

        for arg in args:
            cmd_class = lcmd_name2class(arg)
            if cmd_class:
                print "'%s' is a valid command, using it\n" % arg
                break
            else:
                print "'%s' is a not a valid command" % arg
        else: # none of the args is a valid command
            title = "Valid command was not specified."
            cmd_class = cmd_class_from_menu(title)

    # one command is specified
    else:
        cmd_name = args[0]
        cmd_class = lcmd_name2class(cmd_name)

        if not cmd_class:
            title = "'%s' is a not a valid command." % cmd_name
            cmd_class = cmd_class_from_menu(title)

    return cmd_class()

# list of supported commands
create_command.sup_cmd_classes = [AdminInstallCommand, AdminRemoveCommand,
                                  UserInstallCommand]

class DynamicImporter(object):
    '''Manages dynamically imported modules. Most of these are modules that are
    not distributed with python. Keeps the imported modules in a dictionary, so
    they could be re-used. If module cannot be imported will attempt to install
    respective package using package manager.

    Package manager is not used to check if the respective package is already
    installed, because the user can install 3rd party modules not using package
    manager.'''

    def __init__(self):
        '''Constructor.'''
        self.__modules = {} # imported modules

        # dictionary of packages that contain modules, if module cannot be
        # imported attempt will be made to install respective package
        self.__packages = {
            'PyQt4.QtGui' : PKG_MGR.pkg_name_pyqt4,
            'PyQt4.QtCore' : PKG_MGR.pkg_name_pyqt4,
            'pexpect' : PKG_MGR.pkg_name_pexpect,
            'pycurl' : PKG_MGR.pkg_name_pycurl
            }

        # import packages that are always needed, Qt packages are only needed
        # when running GUI
        self["pycurl"]

    def __do_import(self, module_name):
        '''Does the actual module import'''
        self.__modules[module_name] = \
            __import__(module_name, globals(), locals(), [''])

    def __import_sb_module(self, module_name):
        '''Imports a Scratchbox python module. This enables a cleaner approach
        than to call sb-conf directly. If this approach ceases to work, will
        have to go back to old school way of using sb-conf to e.g. list
        sessions, then grep, count lines etc.
        module_name = name of the module, check help on __import__ for
                      more info'''

        sb_py_path = "%s/tools/lib/python2.3/" % (SB_PATH,)

        try:
            sys.path.append(sb_py_path)
            self.__do_import(module_name)
            sys.path.remove(sb_py_path)
        except ImportError, e:
            raise FriendlyException(
                "Failed to import %s. Please complete %s installation first." %
                (module_name, SB_NAME))
 
    def __import_module(self, module_name):
        '''Imports a module. If module cannot be imported will attempt to
        install respective package. Raises FriendlyException if package
        installation fails. The caller may catch the exception and implement
        some sort of workaround.

        module_name = name of the module, check help on __import__ for
                      more info'''

        # attempt to import
        try:
            self.__do_import(module_name)

        # the module is missing, try to install package
        except ImportError:

            package_name = self.__packages.get(module_name)
            
            if not package_name:
                raise FriendlyException(
                    "Failed to import %s and don't know how to install it! "
                    "You might have to install those modules yourself." %
                    module_name)

            self_install_msg = (
                "%s package is needed but not installed. Please install "
                "it then try again." % package_name)

            # package manager is not available on this system
            if isinstance(PKG_MGR, UnknownPkgManager):
                raise FriendlyException(self_install_msg)

            # package installation can only be done as root
            if not running_as_root():
                txt = sc.user_fail_refer_admin(AdminInstallCommand.name)
                raise FriendlyException("%s\n\n%s" % (self_install_msg, txt))

            # user chose not to install the package
            if not is_yes("Package %s is needed but not installed, install?"
                          % package_name):
                raise FriendlyException("Not installing missing package %s" %
                                        package_name)

            # user chose to install the package
            try:
                print
                PKG_MGR.pkg_install(package_name)
                self.__do_import(module_name)
            except Exception, e: # package manager failed
                MSGR.log_exc()
                raise FriendlyException(
                    "Failed installing %s. See %s for details. Please "
                    "install %s packages yourself." %
                    (package_name, MSGR.fn_log, module_name))

    def get_module(self, module_name):
        '''Returns requested module if module already imported. If module is
        not imported yet, will import it and then return it. Might raise
        exceptions as mentioned in __import_module.

        module_name = see __import_module'''

        if module_name not in self.__modules:
            MSGR.log("Requested module %s not imported, will try to import" %
                     module_name)
            if module_name.startswith("sb."):
                self.__import_sb_module(module_name)
            else:
                self.__import_module(module_name)

        else:
            pass
#            MSGR.log("Requested module %s already imported" % module_name)

        return self.__modules[module_name]

    def __getitem__(self, module_name):
        '''Just a convenience method'''
        return self.get_module(module_name)

class SysInfo(object):
    '''Some information about operating system'''

    def __init__(self):
        '''Constructor. Will raise FriendlyException if system is not
        supported.'''
        self.os = None # OS type (e.g. linux)
        self.machine = None # OS machine type (e.g. i686)
        self.distro_id = None # lower-cased Linux distributor ID (e.g. ubuntu)
        self.distro_release = None # distribution release number (e.g. 9.04)
        self.distro_codename = None # lower-cased distribution release code name (e.g. jaunty)
        self.has_scratchbox = False # whether host already has scratchbox installed
        self.has_unsup_vdso = False # whether current VDSO settings are unsupported
        self.vdso_var = "" # name of VDSO variable
        self.vdso_val = 0 # correct value of VDSO variable
        self.has_unsup_selinux = False # whether current SELinux mode is unsupported (Enforcing)
        self.has_unsup_mmap_mina = False # whether current mmap_min_addr settings are unsupported
        self.mmap_mina_var = "vm.mmap_min_addr" # name of mmap_min_addr variable
        self.mmap_mina_val = 4096 # correct value of mmap_min_addr variable (according to legacy installers)
        self.sysctl_conf_fn = "/etc/sysctl.conf" # location of the sysctl.conf file

        self.is_64_bit = False           # True if OS is 64 bit 

        self.__get_info()
        self.__check_if_supported()

    def __is_mmap_mina_unsupported(self):
        '''Returns True if mmap_min_addr settings of kernel are unsupported by
        QEMU.

        On Lucid the value cannot be read by qemu, so must make sure it is
        correct, see:
        https://bugs.launchpad.net/ubuntu/+source/linux/+bug/568844
        '''
        # if not Lucid, then mmap_min_addr is supported
        if self.distro_codename != "lucid":
            return False

        val = get_kernel_param(self.mmap_mina_var, int)

        if val == None: # failed reading
            return False

        if val > self.mmap_mina_val:
            return True
        else:
            return False

    def __is_vdso_unsupported(self):
        '''Returns tuple of boolean and string. Boolean will be True if VDSO
        settings of kernel are unsupported by Scratchbox, the string is the
        name of the VDSO variable used by the kernel. Different kernels can use
        different variable names for VDSO.

        If none of the known VDSO variables are set, then it is assumed that
        VDSO settings are supported.'''

        # default values assume VDSO settings are supported
        result_has_unsup_vdso = False
        result_vdso_var = ""

        # possible VDSO kernel variables
        vdso_vars = ["vm.vdso_enabled",
                     "abi.vsyscall32",  # used on 64 bit systems
                     "kernel.vdso"]

        # values of VDSO variables supported by scratchbox
        supported_vdso_list = [0, 2]

        for vdso_var in vdso_vars:
            val = get_kernel_param(vdso_var, int)

            if val == None: # failed reading
                continue

            # got int value of VDSO variable, it is supported if in the list
            else:
                result_has_unsup_vdso = val not in supported_vdso_list
                result_vdso_var = vdso_var
                break

        return (result_has_unsup_vdso, result_vdso_var)

    def __is_selinux_unsupported(self):
        '''Returns True if SELinux is enabled and currently is in the Enforcing
        mode, which is not supported by scratchbox. Scratchbox currently
        supports only the Permissive SELinux mode.'''
        # check if SELinux is enabled
        try:
            returncode = subprocess_Popen(["selinuxenabled"]).wait()
        except OSError, e:
            MSGR.log("'%s' while checking SELinux, assuming not enabled"
                     % (str(e)))
            return False

        MSGR.log("selinuxenabled returned %s" % (returncode))

        if returncode: # not enabled
            return False

        # SELinux is enabled, now get the mode
        mode = subprocess_Popen(["getenforce"],
                                stdout = subprocess.PIPE
                                ).communicate()[0].strip()

        MSGR.log("getenforce returned %s" % (mode))

        if mode == "Enforcing":
            return True
        else:
            return False

    def __get_info(self):
        '''Gets the OS info. Raises Exception on failure.'''
        self.os = platform.system().lower()

        # linux specific stuff
        if self.os == "linux":

            self.machine = platform.machine()
            
            platform_dist = platform.dist()
            self.distro_id = platform_dist[0].lower()
            self.distro_release = platform_dist[1]
            self.distro_codename = platform_dist[2].lower()

        # non-linux system, to be implemented when needed
        else:
            pass

        self.is_64_bit = platform.machine() in ["x86_64"]

        self.has_scratchbox = os.path.isfile("%s/etc/scratchbox-version"
                                             %(SB_PATH))

        if TESTING_UI: # when testing all pages shall be shown
            self.has_unsup_vdso = True
            self.has_unsup_selinux = True
            self.has_unsup_mmap_mina = True
        else:
            # vdso is now supported by scratchbox
            # (self.has_unsup_vdso, self.vdso_var) = \
            #     self.__is_vdso_unsupported()
            self.has_unsup_selinux = self.__is_selinux_unsupported()
            self.has_unsup_mmap_mina = self.__is_mmap_mina_unsupported()

        MSGR.log("Got system info: %s" % self)

    def __check_if_supported(self):
        '''Checks if the system is supported by this script. Raises
        FriendlyException if not.'''

        # check OS: currently only Linux is supported
        if self.os != "linux":
            raise FriendlyException(
                "Operating system '%s' is not supported.\nOnly Linux is "
                "supported currently." % self.platform)

        MSGR.log("System check done... system is supported")

    def __str__(self):
        '''String representation method.'''
        return str(self.__dict__)

class UsersSelections(object):
    '''Selections made by the user in the UI.'''

    def __init__(self):
        '''Constructor.'''
        # names of users for whom to install/remove the targets
        self._usernames = set()
        self._usernames.add(get_default_username())
        self.handle_usernames_update()

    def get_usernames(self):
        '''Usernames getter. Returns shallow copy, so that changes made to the
        returned value don't change the member usernames.'''
        return self._usernames.copy()

    def set_usernames(self, new_usernames):
        '''Usernames setter'''
        if new_usernames != self._usernames:  # same set no need to update
            self._usernames = set(new_usernames)
            self.handle_usernames_update()

    usernames = property(get_usernames, set_usernames)

    def handle_usernames_update(self):
        '''Called whenever usernames are updated'''
        pass

    def __str__(self):
        '''String representation method.'''
        return str(self.__dict__)

    def get_summary_text(self, newline = '\n', bold_begin = '', bold_end = ''):
        '''Return a string with the selections summary.
        newline = newline character to be used in the string
        bold_begin = beginning tag of the bold text
        bold_end = ending tag of the bold text'''
        pass

class InstallSelections(UsersSelections):
    '''User's selections in the install UI.'''

    def __init__(self):
        '''Constructor.'''

        if SYS_INFO.has_unsup_vdso:
            # if True, VDSO must be permanently set to SB supported value
            self.set_perm_vdso = False

        if SYS_INFO.has_unsup_selinux:
            # if True, SELinux mode must be permanently set to Permissive
            self.set_perm_selinux = False

        if SYS_INFO.has_unsup_mmap_mina:
            # if True, mmap_min_addr must be permanently set to QEMU supported
            # value
            self.set_perm_mmap_mina = False

        # if targets don't exist they will be created with default names so
        # following variables are useless
        self.targets_remove = False # shall remove existing target, or not

        # target name prefix, used to create new target if not removing
        # existing ones
        self.targets_prefix = ""

        # Not really a selection, updated each time user names set is
        # updated. For users that already have installation targets will store
        # user name as key and a list of two Boolean flags as a value. The
        # first flags is True if x86 target exists and the second one if armel
        # target exists.
        self.existing_targets = {}

        # TODO https_proxy for extranet???
        self.proxy_env_var = "http_proxy" # proxy environment variable
        self.proxy = os.getenv(self.proxy_env_var, "") # the proxy server

        self.display = ":2"

        UsersSelections.__init__(self)

    targets_exist = property(lambda self: bool(self.existing_targets),
                             doc = "True if installation targets already "
                             "exist for any of the selected users")

    def handle_usernames_update(self):
        '''Updates the existing_targets dictionary to reflect currently
        selected usernames.'''

        UsersSelections.handle_usernames_update(self)

        self.existing_targets.clear()
        
        for username in self._usernames:
            target_x86_exist = sb_target_exists(username, TARGET_X86.name)
            target_armel_exist = sb_target_exists(username, TARGET_ARMEL.name)

            # if either exists add to the dictionary
            if target_x86_exist or target_armel_exist:
                self.existing_targets[username] = [target_x86_exist, target_armel_exist]

    def get_summary_text(self, newline = '\n', bold_begin = '', bold_end = ''):
        '''See docstring of the same method in the parent class'''

        text = ""

        # VDSO summary 
        if SYS_INFO.has_unsup_vdso:
            text += "%s%s%s %s%s" % (bold_begin,
                                     si.summary_txt_vdso,
                                     bold_end,
                                     bool_to_yesno(self.set_perm_vdso),
                                     newline)
            text += newline

        # SELinux summary 
        if SYS_INFO.has_unsup_selinux:
            text += "%s%s%s %s%s" % (bold_begin,
                                     si.summary_txt_selinux,
                                     bold_end,
                                     bool_to_yesno(self.set_perm_selinux),
                                     newline)
            text += newline

        # mmap_min_addr summary 
        if SYS_INFO.has_unsup_mmap_mina:
            text += "%s%s%s %s%s" % (bold_begin,
                                     si.summary_txt_mmap_mina,
                                     bold_end,
                                     bool_to_yesno(self.set_perm_mmap_mina),
                                     newline)
            text += newline

        # users summary
        text += "%s%s%s%s" % \
            (bold_begin, si.summary_txt_users, bold_end, newline)
        text += seq_to_str(self.usernames, newline, newline)

        # these are only printed if any users are chosen
        if self.usernames:

            # if any targets exist
            if self.targets_exist:
                text += newline

                # overwriting or not
                text += "%s%s: %s%s%s" % (
                    bold_begin,
                    si.targets_txt_overwrite,
                    bold_end,
                    bool_to_yesno(self.targets_remove),
                    newline)

                # target name prefix
                if not self.targets_remove:
                    text += newline
                    text += "%s%s: %s%s%s" % (bold_begin,
                                              si.summary_txt_targets_prefix,
                                              bold_end,
                                              self.targets_prefix,
                                              newline)

        return text

class RemoveSelections(UsersSelections):
    '''User's selections in the remove UI.'''

    def __init__(self):
        '''Constructor.'''

        UsersSelections.__init__(self)

    def get_summary_text(self, newline = '\n', bold_begin = '', bold_end = ''):
        '''See docstring of the same method in the parent class'''
        text = ""

        text += (
            "%(bold_begin)sWARNING!%(bold_end)s The '%(sb_path)s' directory, "
            "all of the underlying directories and their contents will be "
            "completely removed. %(bold_begin)sThis means that user home "
            "directories in %(sb_name)s will also be removed!%(bold_end)s " %
            {"nl" : newline, "bold_begin" : bold_begin, "bold_end" : bold_end,
             "sb_path" : SB_PATH, "sb_name" : SB_NAME, "my_name" : MY_NAME })
             
        if PRODUCT_FILES:
            text += (
                "Additionally, the following files, installed by the "
                "%(my_name)s will be removed:%(nl)s%(nl)s%(files)s%(nl)sUpon "
                "completion of the removal it will not be possible to recover "
                "any of those files." % {"my_name" : MY_NAME, "nl" : newline,
                 "files" : seq_to_str([f.path for f in PRODUCT_FILES],
                                      newline, newline, False)})
        text = get_wrapped(text)
        text += newline
        text += newline

        # scratchbox
        text += "%s%s%s %s%s" % (bold_begin,
                                 sr.summary_txt_scratchbox,
                                 bold_end,
                                 bool_to_yesno(True),
                                 newline)
        # the targets
        text += "%s%s%s %s%s" % (bold_begin,
                                 sr.summary_txt_targets,
                                 bold_end,
                                 bool_to_yesno(True),
                                 newline)
        # scratchbox homes
        text += "%s%s%s %s%s" % (bold_begin,
                                 sr.summary_txt_sb_homes,
                                 bold_end,
                                 bool_to_yesno(True),
                                 newline)
        # files
        if PRODUCT_FILES:
            text += "%s%s%s %s%s" % (bold_begin,
                                     sr.summary_txt_files,
                                     bold_end,
                                     bool_to_yesno(True),
                                     newline)

        return text

class PkgManager(object):
    '''System independent interface to the package manager. This is an abstract
    base class'''

    CmdInstall = 0
    CmdRemove = 1

    def __init__(self):
        '''Constructor.'''
        # these are set in child classes
        self.name = None     # name of the package manager for the UI text
        self.src_dir = None  # repository sources directory
        self.src_file = None # name of the repository source file
        self.repo = ""       # product (Scratchbox) repository

        # name of the product (Scratchbox) installation package, a list of
        # names if there are many packages
        self.pkg_name_product = None
        self.pkg_desc_product = SB_NAME # if set will be displayed instead of package name when installing/removing
        self.pkg_name_pyqt4 = None # name of the PyQt4 package
        self.pkg_name_pexpect = None # name of the pexpect package
        self.pkg_name_pycurl = None # name of the curl bindings package
        self.pkg_name_xephyr = None # name of the xephyr server package
        self.pkg_name_https_transport = None # name of the package managers
                                             # HTTPS transport package

    def __run_cmd(self, cmd, name, desc, fatal):
        '''Runs a package manager command.
        cmd = command to run
        name = string name of the package to install, if it is a list all
               packages from the list will be installed
        desc = description, will be used to say, if not specified package
               name is used instead
        fatal = True if the installation process should be executed as a
                fatal command'''
        cmd_data = {self.CmdInstall : ("Installing %s package%s", self._pkg_install),
                    self.CmdRemove :  ("Removing %s package%s", self._pkg_remove)}

        cmd_say_msg = cmd_data[cmd][0]
        cmd_meth = cmd_data[cmd][1]

        # convert list of names to string
        if isinstance(name, (list, tuple)):
            name = " ".join(name)
            pl = "s"
        else:
            pl = ""

        if not desc:
            desc = name

        try:
            self.verify_is_unlocked()
            MSGR.say(cmd_say_msg % (desc, pl), MSGR.SayTypeTask)
            cmd_meth(name, fatal)

        finally:
            MSGR.cli_progress_stop()

    def pkg_install(self, name, desc = None, fatal = True):
        '''Installs a package. Children should not overridde this method but
        provide one starting with underscore instead. For details see the
        __run_cmd method.'''
        self.__run_cmd(self.CmdInstall, name, desc, fatal)

    def pkg_remove(self, name, desc = None, fatal = True):
        '''Removes a package. Children should not overridde this method but
        provide one starting with underscore instead. For details see the
        __run_cmd method.'''
        self.__run_cmd(self.CmdRemove, name, desc, fatal)

    def _pkg_install(self, name, fatal = True):
        raise AbstractMethodException()

    def _pkg_remove(self, name, fatal = True):
        raise AbstractMethodException()

    def pkg_is_installed(self, name):
        '''Returns True if package is installed, False otherwise. Children
        should overridde this method.
        name = name of the package'''
        raise AbstractMethodException()

    def is_locked(self):
        '''Returns True if package manager is locked by another process. False
        otherwise. Children should overridde this method.'''
        raise AbstractMethodException()

    def verify_is_unlocked(self):
        '''Upon return of this method the package manager will not have any
        locks.'''
        while self.is_locked():

            answer = MSGR.ask(
                "",
                "It appears %s is locked, please close any applications using "
                "%s, then retry!" % (self.name, self.name),
                [ASK_CHOICE_RETRY, ASK_CHOICE_ABORT], ASK_CHOICE_RETRY)

            if answer == ASK_CHOICE_ABORT:
                raise FriendlyException("Could not use %s because of lock!" %
                                        (self.name,))

    def repo_requires_authentication(self):
        '''Returns True if authentication is needed to access the repository'''
        return self.repo.find("https") != -1

    def get_repo_uri(self):
        '''Returns repository URI, that's gets rid of deb distribution and
        component parts of the repo. It is assumed that repo format is
        according to man 'sources.list':
        deb uri distribution [component1] [component2] [...]
        Hence, currently only Debian repository entries are supported.'''
        return self.repo[self.repo.index("https"):self.repo.rindex(" ")]

    def repo_install(self):
        '''Adds product repository to the list of package manager repository
        sources'''
        MSGR.say("Installing repository file %s" % self.src_file,
                 MSGR.SayTypeTask)

        if not os.path.isdir(self.src_dir):
            raise FriendlyException(
                "There is no %s on this system! The package manager cannot "
                "be sane!" % (self.src_dir,))

        # not https repo: leave as is
        if not self.repo_requires_authentication():
            repo = self.repo

        # https repo: add credentials to it
        else:
            creds = CREDS_MGR.get_creds(self.get_repo_uri())

            username = creds[0]
            password = creds[1]
 
            repo = self.repo.replace("https://", "https://%s:%s@" %
                                     (username, urllib.quote(password, "")))

        repo_lines = [
            "# This is the %s repository for the %s" % (SB_NAME, PRODUCT_NAME),
            repo
            ]

        file_add_lines(self.src_file, repo_lines, append = False,
                       log_lines = True)

    def repo_remove(self):
        '''Removes product repository from the list of package manager
        repository sources'''
        MSGR.say("Removing repository file %s" % self.src_file,
                 MSGR.SayTypeTask)

        try:
            os.remove(self.src_file)
        except OSError, e:
            MSGR.warn("Repository source file does not exist (%s)" % e)

        MSGR.cli_progress_stop()

    def product_install(self):
        '''Installs all the product packages'''
        self.repo_install()
        self.pkg_install(self.pkg_name_product, self.pkg_desc_product)

    def product_remove(self):
        '''Removes all the product packages'''
        self.pkg_remove(self.pkg_name_product, self.pkg_desc_product)
        self.repo_remove()

    def _exec_cmd(self, cmd, fatal):
        '''Executes a command.
        cmd = command to execute
        fatal = if True then command will be executed as critical'''
        if fatal:
            exec_cmd_fatal(cmd)
        else:
            exec_cmd_error(cmd)

class AptPkgManager(PkgManager):
    '''Interface to the Debian apt package manager'''

    def __init__(self):
        '''Constructor.'''
        PkgManager.__init__(self)

        self.name = "apt"
        self.src_dir = "/etc/apt/sources.list.d"
        self.src_file = os.path.join(self.src_dir,
                                     PRODUCT_NAME_FILES + ".list")

        self.repo = SB_DEB_REPO
        self.pkg_name_product = SB_DEB_PACKAGES
        self.pkg_name_pyqt4 = "python-qt4"
        self.pkg_name_pexpect = "python-pexpect"
        self.pkg_name_pycurl = "python-pycurl"
        self.pkg_name_xephyr = "xserver-xephyr"
        self.pkg_name_https_transport = "apt-transport-https"

    def pkg_is_installed(self, name):

        status = subprocess_Popen(["dpkg", "-s", name],
                                  stdout = subprocess.PIPE,
                                  stderr = subprocess.STDOUT
                                  ).communicate()[0].strip()

        MSGR.log("Package %s status:\n%s" % (name, status))

        p = re.compile("Status: install ok installed")

        for line in status.splitlines():
            m = p.match(line)

            if m:
                return True

        return False

    def is_locked(self):
        return exec_cmd("apt-get check -qq")

    def _pkg_install(self, name, fatal = True):
        self._exec_cmd("apt-get update -qq", fatal)
        self._exec_cmd("apt-get install %s --force-yes -y" % name, fatal)

    def _pkg_remove(self, name, fatal = True):
        self._exec_cmd("apt-get remove %s -y" % name, fatal)

        # remove useless dependencies, this will silently fail on systems that
        # don't support 'autoremove' (e.g. Debian Etch)
        exec_cmd("apt-get autoremove -y")

class YumPkgManager(PkgManager):
    '''Interface to the yum package manager'''

    def __init__(self):
        '''Constructor.'''
        PkgManager.__init__(self)

        self.name = "yum"
        self.src_dir = "/etc/yum.repos.d"
        self.src_file = os.path.join(self.src_dir,
                                     PRODUCT_NAME_FILES + ".repo")

        self.pkg_name_pyqt4 = "PyQt4"
        self.pkg_name_pexpect = "pexpect"
        self.pkg_name_pycurl = "python-pycurl"
        self.pkg_name_xephyr = "xorg-x11-server-Xephyr"

    def pkg_is_installed(self, name):

        returncode = exec_cmd("yum list installed %s" % (name,))

        if returncode:
            return False
        else:
            return True

    def is_locked(self):
        # seems file will exist while yum is locked
        return os.path.isfile("/var/run/yum.pid")

    def _pkg_install(self, name, fatal = True):
        self._exec_cmd("yum install %s -y" % name, fatal)

    def _pkg_remove(self, name, fatal = True):
        self._exec_cmd("yum remove %s -y" % name, fatal)

class ZyppPkgManager(PkgManager):
    '''Interface to the ZYpp package manager of OpenSuse. Uses command line
    interface zypper.'''

    def __init__(self):
        '''Constructor.'''
        PkgManager.__init__(self)

        self.name = "zypp"
        self.src_dir = "/etc/zypp/repos.d/"
        self.src_file = os.path.join(self.src_dir,
                                     PRODUCT_NAME_FILES + ".repo")
        self.pkg_name_pyqt4 = "python-qt4"
        self.pkg_name_pexpect = "python-pexpect"
        self.pkg_name_pycurl = "python-curl"
        self.pkg_name_xephyr = "xorg-x11-server-extra"

    def pkg_is_installed(self, name):

        status = subprocess_Popen(["zypper", "info", name],
                                  stdout = subprocess.PIPE,
                                  stderr = subprocess.STDOUT
                                  ).communicate()[0].strip()

        MSGR.log("Package %s status:\n%s" % (name, status))

        p = re.compile("Installed: Yes")

        for line in status.splitlines():
            m = p.match(line)

            if m:
                return True

        return False

    def is_locked(self):
        return os.path.isfile("/var/run/zypp.pid")

    def _pkg_install(self, name, fatal = True):
        self._exec_cmd("zypper in -y %s" % name, fatal)

    def _pkg_remove(self, name, fatal = True):
        self._exec_cmd("zypper rm -y %s" % name, fatal)

class UnknownPkgManager(PkgManager):
    '''Package manager interface created on systems that have no known package
    manager. This class just prints errors messages if asked to install
    packages.'''

    def __init__(self):
        '''Constructor.'''
        PkgManager.__init__(self)

        self.src_dir = "None"
        self.src_file = os.path.join(self.src_dir,
                                     PRODUCT_NAME_FILES + ".repo")
        self.pkg_name_pyqt4 = "Python Qt4 bindings"
        self.pkg_name_pexpect = "pexpect"
        self.pkg_name_pycurl = "Python cURL bindings"
        self.pkg_name_xephyr = "Xephyr X server"

    def pkg_is_installed(self, name):
        return True

    def is_locked(self):
        return False

    def _pkg_install(self, name, fatal = True):
        raise FriendlyException("Please install package %s..." % (name))

    def _pkg_remove(self, name, fatal = True):
        raise FriendlyException("Please remove package %s..." % (name))

def pkg_manager():
    '''System dependent package manager factory function'''

    if SYS_INFO.distro_id in ["ubuntu", "debian", "linuxmint"]:
        return AptPkgManager()
    elif SYS_INFO.distro_id in ["fedora", "redhat"]:
        return YumPkgManager()
    elif SYS_INFO.distro_id in ["suse"]:
        return ZyppPkgManager()
    else:
        return UnknownPkgManager()

class ProcessWrapper(object):
    '''Wrapper around the process creation classes. It is made to store the PID
    of the running process, so the setup could kill it (including its children,
    i.e. the whole process group) when needed. Although the process classes
    (subprocess and pexpect) have kill method, they do not kill the whole
    group.'''

    def __init__(self, process_class, *args, **kwds):
        '''Constructor.'''

        # attributes can be set for this object using dot notation until this
        # flag is not set
        self.__dict__['__initialized'] = False

        self.__process_class = process_class
        MSGR.log("Creating process %s: %s %s" %
                 (self.__process_class, args, kwds))

        self.__process_object = None
        
        try:
            self.__process_object = process_class(*args, **kwds)

        except Exception, e:
            
            if False: # logging to be done in callers
                MSGR.log_exc()
                MSGR.log(str(e))

                if hasattr(e, 'child_traceback'): # subprocess.Popen stuff
                    MSGR.log("child_traceback:")
                    MSGR.log(e.child_traceback)

            raise

        MSGR.log("Created %s with pid = %s" %
                 (self.__process_class, self.__process_object.pid))

        global CHILD_PIDS
        CHILD_PIDS.add(self.__process_object.pid)


        self.__dict__['__initialized'] = True

    def __del__(self):
        '''Destructor.'''
        global CHILD_PIDS

        if self.__process_object:

            assert self.__process_object.pid in CHILD_PIDS, \
                "PID (%s) of running child should be in the list" % \
                (self.__process_object.pid)

            status = "Unknown"
            if hasattr(self.__process_object, "returncode"): # subprocess.Popen
                status = self.__process_object.returncode 
            elif hasattr(self.__process_object, "exitstatus"): # pexpect
                status = self.__process_object.exitstatus

            MSGR.log("Destroying %s with pid = %s, exit status = %s" %
                     (self.__process_class, self.__process_object.pid,
                      status))
            CHILD_PIDS.remove(self.__process_object.pid)
        else:
            MSGR.log("Destroying %s with no object whatsoever!" %
                     (self.__process_class))

    def __getattr__(self, attr):
        '''Delegates functionality to the process object'''
        return getattr(self.__process_object, attr)

    def __setattr__(self, attr, val):
        '''If this object is not initialized or it has the specified attribute,
        that attribute of this object will be set. Otherwise the attribute of
        the delegated object will be set. NOTE: This means, after the
        initialization attributes cannot be added to this object using the dot
        notation.'''
        if not self.__dict__['__initialized'] or \
                self.__dict__.has_key(attr):
            object.__setattr__(self, attr, val)

        else:
            setattr(self.__process_object, attr, val)

def subprocess_Popen(*args, **kwds):
    '''Wrapper around subprocess.Popen. If a process is created using this
    wrapper, it will be killed if setup quits before completion.'''
    return ProcessWrapper(subprocess.Popen, *args, **kwds)

def pexpect_spawn(*args, **kwds):
    '''Wrapper around pexpect.spawn. If a process is created using this
    wrapper, it will be killed if setup quits before completion.'''
    pexpect = IMPORTER['pexpect']
    return ProcessWrapper(pexpect.spawn, *args, **kwds)

def signal_all(signum):
    '''Sends a signal to process groups of all of the children belonging to the
    different group than the script. Then the same signal is sent to the
    process group of the script. The order is such so that all different
    children groups are killed before the scripts group is killed.'''
    MSGR.log("About to send signal %s to self and children %s" %
             (signum, CHILD_PIDS))

    my_pgid = os.getpgid(os.getpid())

    for child_pid in CHILD_PIDS:

        try:
            child_pgid = os.getpgid(child_pid)

        # it is possible for python object to exist (e.g. because it is not
        # garbage collected), though the process has already died
        except OSError, e:
            MSGR.warn("Child group %d does not exist (%s)" % (child_pid, e))
            continue

        if child_pid != my_pgid:
            MSGR.log("Sending signal %s to child group pid = %s, pgid = %s" %
                     (signum, child_pid, child_pgid))
            os.killpg(child_pgid, signum)

    MSGR.log("Sending signal %s to my group %s" % (signum, my_pgid))
    os.killpg(my_pgid, signum)

def kill_all():
    '''If there are children processes, kills their groups then the group of
    the script, otherwise just quits.'''
    if CHILD_PIDS:
        MSGR.log("Committing suicide...")
        signal_all(signal.SIGTERM)
        time.sleep(1)
        signal_all(signal.SIGKILL)
    else:
        sys.exit(100)

def exec_cmd(command, username = None, env = None):
    '''Executes a command and returns the result. Since the command is executed
    via shell exception is not raised if command is not in path. Instead shell
    will return error code.
    username = If set, the command will be executed with the credentials
               (UID & GID) of that user.
    env = If set, defines the environment variables for the command.'''

    # if root privileges not needed, specify function to remove root privileges
    if username:
        MSGR.log("Executing as user %s: %s" % (username, command))

        # root cannot run scratchbox commands, sbox group is needed to run them
        need_sbox_group = False
        if command.startswith(SB_PATH):
            need_sbox_group = True

        child_preexec_fn = lambda: set_guid(username, need_sbox_group)

    else:
        MSGR.log("Executing: %s" % (command,))
        child_preexec_fn = None

    p = subprocess_Popen("exec " + command,
                         stdout = MSGR.fo_log,
                         stderr = subprocess.STDOUT,
                         preexec_fn = child_preexec_fn,
                         shell = True,
                         env = env)
    p.wait()

    return p.returncode

def get_sudo_env_str():
    '''Returns an env string for sudo command. That string will set needed
    enviroment variables for sudo. By default sudo removes most of the
    environment variables, so that is why this env workaround is used.'''
    env_str = ""
    
    # environment variables to pass to sudo
    env_vars = ["https_proxy", "http_proxy"]

    for var in env_vars:
        if os.getenv(var):
            env_str += " %s=%s" % (var, os.getenv(var))

    if env_str:
        env_str = "env" + env_str

    return env_str

def exec_cmd_creds(command, username = None, env = None):
    '''Executes a command and sends to it repo info credentials if the command
    asks. Returns the return code of the comand.
    username = If set, the command will be executed with the credentials
               (UID & GID) of that user.
    env = If set, defines the environment variables for the command.'''
    pexpect = IMPORTER['pexpect']

    if username: # run as user
        env_str = get_sudo_env_str()
        command = "sudo -H -u %s %s " % (username, env_str) + command

    creds = CREDS_MGR.get_creds()
    username = creds[0]
    password = creds[1]

    MSGR.log("Executing: %s" % command)

    timeout_s = 6000 # in seconds, should be enough for most tasks

    e = pexpect_spawn(command, logfile = MSGR.fo_log, timeout = timeout_s,
                      env = env)

    index = e.expect(['Username:', pexpect.EOF, pexpect.TIMEOUT])

    if index == 0: # credentials asked
        e.logfile = None # turn off logging, not to show username/password
        e.sendline(username)
        e.expect('Password:')
        e.sendline(password)
        e.logfile = MSGR.fo_log
        e.expect(pexpect.EOF)

    elif index == 1: # EOF (e.g. creds were not asked, used from netrc)
        MSGR.log("EOF while expecting")

    elif index == 2: # TIMEOUT
        MSGR.warn("TIMEOUT while expecting")

    e.close()

    returncode = e.exitstatus

    if returncode == None: # timeout or other failure
        MSGR.warn("Expect exit status is None, assuming failure")
        returncode = 1

    return returncode

class CmdFatalFailure(Exception):
    '''Exception raised when execution of a fatal command fails.'''
    pass

class CmdErrorFailure(Exception):
    '''Exception raised when execution of a non-critical command fails.'''
    pass

def exec_cmd_fatal(command, username = None, env = None, send_creds = False):
    '''Executes a critical command and raises exception if command execution
    fails. Critical commands are the ones that must succeed in order for the
    setup to go on.
    username = If set, the command will be executed with the credentials
               (UID & GID) of that user.
    send_creds = set to True if command might ask repo credentials, so
                 credentials will be send to the command'''
    if send_creds:
        returncode = exec_cmd_creds(command, username, env)
    else:
        returncode = exec_cmd(command, username, env)

    if returncode:
        raise CmdFatalFailure("Giving up, because failed to: %s" % command)

def exec_cmd_error(command, username = None, env = None, send_creds = False):
    '''Executes a command and stores an error if command execution fails. This
    function should be used to execute commands that are not critical, i.e. if
    they fail the setup can continue.
    username = If set, the command will be executed with the credentials
               (UID & GID) of that user.
    send_creds = set to True if command might ask repo credentials, so
                 credentials will be send to the command'''
    if send_creds:
        returncode = exec_cmd_creds(command, username, env)
    else:
        returncode = exec_cmd(command, username, env)

    if returncode:
        MSGR.add_error()
        raise CmdErrorFailure("Error executing: %s" % command)

def cut_str(var):
    '''Reduces the length of the passed in string and returns the cut
    string. If the passed in variable is not a string, nothing is done. The
    length of strings is normally reduced for logging, e.g. to avoid logging
    whole file contents.'''
    max_str_len = 160

    # var is a too long string
    if isinstance(var, str) and len(var) > max_str_len:
            return var[:max_str_len] + "..."

    return var

def cut_strs_in_container(cont):
    '''Returns copy of the container with strings cut.'''

    # container is a sequence
    if isinstance(cont, (list, tuple)):
        out = []

        for i in cont:
            out.append(cut_str(i))

    # container is a mapping
    elif isinstance(cont, dict):
        out = cont.copy()

        for k in out.iterkeys():
            out[k] = cut_str(out[k])
           
    else:
        raise Exception("Unsupported type!!!")

    return out

def seq_to_str(seq, sep = '\n', last_sep = '\n', sort = True):
    '''Convers a sequence into a string. Returns the new string. If there are
    no items in the sequence then None is returned.
    sep = character to separate sequence items in the string
    last_sep  = character to be used after the last item
    sort = whether to sort the sequence before printing'''

    if not seq:
        return "None" + sep

    else:

        text_str = ''

        if sort:
            fseq = sorted(seq)
        else:
            fseq = list(seq) # convert sets etc. to list to index later on

        for i, v in enumerate(fseq):
            if i == len(fseq) - 1:
                nl = last_sep
            else:
                nl = sep

            text_str += v + nl

    return text_str

def bool_to_yesno(bool_value):
    '''Return yes if Boolean value is true, no if it is false'''
    if bool_value:
        return 'Yes'
    else:
        return 'No'

class MyTextWrapper(textwrap.TextWrapper):
    '''Child of TextWrapper that can wrap multiple paragraph in one string'''

    def __init__(self):
        '''Constructor.'''
        # break_long_words: URLs can be long, so breaking them disabled
        # break_on_hyphens: log file name contains hyphens, so disabled
        textwrap.TextWrapper.__init__(self,
                                      break_long_words = False)

        # new in Python 2.6
        if hasattr(self, "break_on_hyphens"):
            self.break_on_hyphens = False

    def wrap_multi(self, text):
        '''Returns list of wrapped paragraphs. Compared to wrap() method,
        this method can wrap multiple paragraphs from a single text string.
        Beginning of a paragraphs is marked by a newline character.'''
        # paragraphs in/out
        pgphs_in = text.splitlines()
        pgphs_out = []
        
        for paragraph in pgphs_in:

            if paragraph: # non-empty string
                wrapped_paragraph = self.wrap(paragraph)
                pgphs_out.extend(wrapped_paragraph)

            else: # empty string, i.e. single newline in original text
                pgphs_out.append(paragraph)

        return pgphs_out

    def fill_multi(self, text):
        '''Returns a string, otherwise same as wrap_multi()'''
        return "\n".join(self.wrap_multi(text))

TEXT_WRAPPER = MyTextWrapper()

def print_wrapped(text):
    '''Prints wrapped text'''
    print get_wrapped(text)

def get_wrapped(text):
    '''Retrurns wrapped copy of text'''
    return TEXT_WRAPPER.fill_multi(text)

def get_default_username():
    '''Returns username of the user that ran this script. If the script is ran
    by root returns the username of the user that invoked sudo or su. If
    running as root and username cannot be found, then will return the first
    available username from all usernames.'''
    if get_default_username.cached_name:
        return get_default_username.cached_name

    # this routine can be (and currently is) used before MSGR is created
    if MSGR: 
        log = MSGR.log
    else:
        # log = sys.stdout.write
        log = lambda msg: None

    username = ""

    uid = os.geteuid()
    
    # running non-root command get name from UID
    if uid != 0:
        username = pwd.getpwuid(uid).pw_name
        log("Got default username from UID: %s" % (username,))

    # running as root have to guess the name
    else:
        log("UID is 0, trying to guess default username")

        all_usernames = get_all_usernames()

        # environment variables to check for usernames
        env_vars = ['SUDO_USER', 'USERNAME']

        for var in env_vars:
            name = os.getenv(var)
            log("Validating username %s from %s" % (name, var))

            # installing SDK as root is not allowed
            if name == 'root':
                log("Invalid: root")
                continue 

            # make sure environment variables have reasonble name
            elif name not in all_usernames:
                log("Invalid: not in all usernames!")
                continue

            else:
                username = name
                log("Username is valid")
                break

        else: # was not found, use first one
            log("Username not found, using first one")
            username = all_usernames[0]

    get_default_username.cached_name = username

    return username

# will be set the first time function is executed, then it will be returned
# with all subsequent calls to save time and be consistent
get_default_username.cached_name = None

def get_default_user_ent():
    '''Returns password database entry for the default user'''
    return pwd.getpwnam(get_default_username())

def ask(title, question, choices, default_choice = None,
        have_choices_in_question = True):
    '''Command line version of ask.
    title = the title
    question = question to be asked (prompt)
    choices = exclusive choices the user can use as an answer
    default_choice = If set, choice to be selected when just return is pressed
    have_choices_in_question = self explanatory

    returns the selected choice'''

    assert choices, "No choices specified"
    assert not default_choice or \
        default_choice in choices, "Default choice not in choices"

    str_choices = "(%s)" % (seq_to_str(choices, "/", "", False))

    # show the default choice
    if default_choice:
        str_choices = str_choices.replace(default_choice,
                                          "[%s]" % default_choice)

    if have_choices_in_question:
        prompt = get_wrapped(question + " " + str_choices)
    else:
        prompt = get_wrapped(question)

    while True: # loop until got answer
        print

        if title:
            print title

        answer = raw_input(prompt + ' ')

        # user just pressed enter, return default answer
        if default_choice and answer == '':
            return default_choice

        if answer in choices:
            return answer

def is_yes(question, default_answer = True):
    '''Ask the user for yes/no answer and returns True if user answered yes
    False if no.
    default_answer = In case user just presses return, if this argument is
                     True, yes will be the default answer, otherwise no will
                     be the default answer'''

    if default_answer:
        default_choice = ASK_CHOICE_YES
    else:
        default_choice = ASK_CHOICE_NO

    answer = ask(None, question, [ASK_CHOICE_YES, ASK_CHOICE_NO],
                 default_choice)

    if answer == ASK_CHOICE_YES:
        return True
    elif answer == ASK_CHOICE_NO:
        return False

def show_eusa():
    '''Shows Nokia EUSA and returns True if the user acceps it, False if the
    user does not accept it.'''

    p = subprocess_Popen("more", stdin = subprocess.PIPE)
    p.communicate(EUSA_TEXT)
    del p  # remove from CHILD_PIDS before interaction

    txt = "Do you accept all the terms of the preceding License Agreement?"
    
    return is_yes(txt, False)

def scan_seq(desc_intro, desc_name, all_items, selected_items):
    '''Shows list of all_items to the user and writes the selections into the
    selected_items set. Also returns the selections.
    desc_intro = introductory string for the list
    desc_name = what an item is called (e.g. user)
    all_items  = list of all items, the user will be selecting from this list
    selected_items = set of items selected by user, can have default values
    '''

    str_continue = 'c'
    str_all = 'a'
    str_none = 'n'

    while True:

        # print introduction
        print "\n"
        print_wrapped(desc_intro)
        print
        print_wrapped("Here is a list of available %ss, selected items are "
                      "marked with an asterisk before the index:" % desc_name)
        print

        # print list
        for index, item in enumerate(all_items):
            if item in selected_items: # mark selected items
                print "*",
            else:
                print " ",
            print index, item,
            print

        # user chose to modify the list
        if is_yes("Would you like to change the selections in the list of %ss?"
                  % desc_name,
                  not len(selected_items)):# if any items: default answer is no

            print

            # read the new selections
            answer = ''
            while answer == '':  # loop until something entered
                print_wrapped("Enter space separated list of integer indices, "
                              "'%s' to select all, '%s' to unselect all, or "
                              "'%s' to continue: " %
                              (str_all, str_none, str_continue))
                answer = raw_input("> ").lower()

            # continue (i.e. quit this list menu)
            if answer == str_continue:
                break

            # select all items
            elif answer == str_all:
                selected_items.update(all_items)

            # clear all items
            elif answer == str_none:
                selected_items.clear()

            # user entered something
            else:
                indices = answer.split()
                cleared = False

                # loop through entered indices
                for ch_index in indices:

                    # convert character to int
                    try:
                        index = int(ch_index)
                    except ValueError:
                        print "Ignoring %s: not an integer index!" % ch_index
                        continue

                    # index within valid range
                    if 0 <= index < len(all_items):

                        # the first time adding, clear the previous entries
                        if not cleared:
                            selected_items.clear()
                            cleared = True

                        selected_items.add(all_items[index])

                    # index out of bounds
                    else:
                        print "Ignoring %s: out of bounds!" % index
                        continue

        # user chose not to modify list
        else:
            break


    return selected_items

def cli_menu(title, items):
    '''Shows a command line single selection menu. Returns the selected item.
    title = title of the menu
    items = items of the menu'''

    selected_item = None # the selected item

    title = title + " Make a choice by entering an integer index:"

    while True:

        print
        print_wrapped(title)
        print

        for index, item in enumerate(items):
            print index, item

        answer = raw_input("> ").lower()

        # convert character to int
        try:
            answer_index = int(answer)
        except ValueError:
            print "Ignoring %s: not an integer index!" % answer
            continue

        # index within valid range
        if 0 <= answer_index < len(items):
            selected_item = items[answer_index]
            break
        else:
            print "Ignoring %s: out of bounds!" % answer_index
            continue

    print

    return selected_item

class UI(object):
    '''Base user interface class'''

    def __init__(self):
        '''Constructor.'''

    def run_cmd(self, cmd):
        '''Reads configuration from the user and then runs specified
        command in the UI. The children should override it to provide
        actual functionality'''

class CmdLineUI(UI):
    '''The command line user interface class.'''

    def __init__(self):
        '''Constructor.'''

    def run_cmd(self, cmd):
        '''See docstring of the same method of the parent class'''

        # look up the correct data
        cmd_data = {
            AdminInstallCommand : [self.__get_install_selections,
                                   InstallSelections],

            UserInstallCommand : [self.__get_install_selections,
                                  InstallSelections],

            AdminRemoveCommand : [self.__get_remove_selections,
                                  RemoveSelections]
            }

        get_selections = cmd_data[cmd.__class__][0]
        users_selections = cmd_data[cmd.__class__][1]()
        s = cmd.s

        # show into and return if the user does not want to continue
        if not self.__show_intro(cmd):
            return

        # loop until got user accepted selections
        while True:
            users_selections = get_selections(users_selections, cmd)

            if self.__read_summary_verification(users_selections, cmd):
                break # summary accepted, run the command

        print
        print
        print_wrapped(
            "To follow the progress please view logs in %(logfile)s\n"
            "For example run the following command in a separate terminal:\n"
            "tail -f %(logfile)s" %
            {"logfile" : MSGR.fn_log})
        print

        cmd.run(users_selections)

        print

        # print error info
        if cmd.status == Command.StatusFatal:
            print
            print sc.fatal_title
            print MSGR.get_fatal()
            print_wrapped(sc.contact_info_on_problems())
            print

        elif cmd.status == Command.StatusErrors:
            print sc.errors_title
            
            for err in MSGR.get_errors():
                print err
            print_wrapped(sc.contact_info_on_problems())
            print

        print sc.summary(s, cmd.status)

    def __show_intro(self, cmd):
        '''Shows intro.'''
        s = cmd.s
        show_license = cmd.is_install # license shown when installing

        # print intro
        print "\n%s!\n" % (s.intro_title)
        print_wrapped(s.intro_subtitle_cli)

        # only root can install the Scratchbox
        if running_as_root() and PKG_MGR.repo_requires_authentication() and \
                hasattr(s, "intro_creds_warn"): # only install has
            print
            print_wrapped(s.intro_creds_warn())

        if show_license: # info about license
            print
            print_wrapped("If you choose to continue %s will be displayed. %s."
                          % (s.license_title, s.license_subtitle))

        answer = is_yes("Would you like to continue?")

        # wants to continue and license must be shown
        if answer and show_license:
            return show_eusa()

        # doesn't want to continue or license shouldn't be shown
        return answer

    def __get_install_selections(self, install_selections, cmd):
        '''Command line user interface for the install command.
        Returns selections.
        install_selections = selections that will be used as default
        cmd = command to get selection for'''

        # VDSO
        if SYS_INFO.has_unsup_vdso:
            print
            print_wrapped(si.vdso_subtitle)
            print
            print_wrapped(si.vdso_info_cli)

            txt = "%s?" % (si.vdso_perm_text,)
            install_selections.set_perm_vdso = is_yes(txt, False)

        # SELinux
        if SYS_INFO.has_unsup_selinux:
            print
            print_wrapped(si.selinux_subtitle)
            print
            print_wrapped(si.selinux_info_cli)

            txt = "%s?" % (si.selinux_perm_text,)
            install_selections.set_perm_selinux = is_yes(txt, False)

        # mmap_min_addr
        if SYS_INFO.has_unsup_mmap_mina:
            print
            print_wrapped(si.mmap_mina_subtitle)
            print
            print_wrapped(si.mmap_mina_info_cli)

            txt = "%s?" % (si.mmap_mina_perm_text,)
            install_selections.set_perm_mmap_mina = is_yes(txt, False)

        # read the users only if running admin install
        if isinstance(cmd, AdminInstallCommand):
            install_selections.usernames = \
                scan_seq(si.users_subtitle,
                         'user',
                         get_all_usernames(),
                         install_selections.usernames)
        
        # targets exist for users, so show some kind of targets page
        if install_selections.targets_exist:
            self.__get_install_targets_selections(install_selections)

        print

        return install_selections

    def __get_install_targets_selections(self, install_selections):
        '''Command line user interface for reading install targets selections.
        install_selections = to read and fill in.'''
        usernames = install_selections.usernames
        
        # first construct and show menu

        # pass in list of usernames who already has targets
        title = si.targets_subtitle(install_selections.existing_targets.keys())
        items = [si.targets_txt_overwrite, si.targets_txt_create]

        # menu to find if user wants to remove targets or create new ones
        selected_item = cli_menu(title, items)

        install_selections.targets_remove = \
            (selected_item == si.targets_txt_overwrite)

        # user chose to create new targets, read the prefix
        if not install_selections.targets_remove:
            print si.targets_create_title
            print
            print_wrapped(si.targets_create_subtitle)

            nonexistent_prefix = sb_get_nonexistent_prefix(usernames)

            while True: # loop till got nonexistent prefix
                txt = get_wrapped("Please specify prefix or press enter "
                                  "to use %s as prefix" %
                                  (nonexistent_prefix,))
                print
                print txt
                prefix = raw_input("> ").lower()

                if prefix == '': 
                    prefix = nonexistent_prefix
                else: # new prefix specified, verity its valid
                    # prefix already used
                    if sb_prefix_exists_any(usernames, prefix):
                        usernames_exists = \
                            sb_get_prefix_exists_usernames(usernames,
                                                           targets_prefix)
                        usernames_str = seq_to_str(usernames_exists, ", ", "")

                        txt = self.s.targets_exists_txt % (targets_prefix,
                                                           usernames_str)

                        print_wrapped(txt)
                        continue

                # prefix is valid
                install_selections.targets_prefix = prefix
                break

    def __get_remove_selections(self, remove_selections, cmd):
        '''Command line user interface for the remove command.
        Returns selections.
        remove_selections = selections that will be used as default
        cmd = command to get selection for'''
        # currently no selection possible
        print "\n"
        return remove_selections

    def __show_summary(self, users_selections, cmd):
        '''Shows summary text.'''
        s = cmd.s
        print "\n%s:\n" % (s.summary_title,)
        print_wrapped(s.summary_subtitle)
        print

        print users_selections.get_summary_text()

    def __read_summary_verification(self, users_selections, cmd):
        '''Shows summary text and asks if the user acceps to start installation or
        removal with these settings. Returns True if the user accepts, False if
        the wants to change the selections'''
        s = cmd.s

        answer_accept = "go"
        answer_change = "change"

        info = get_wrapped("Enter '%s' to begin %s with these settings" %
                           (answer_accept, s.name_noun))

        if not cmd.is_remove: # currently remove has no selections
            info += "\n" + get_wrapped("Enter '%s' to change these settings" %
                                       (answer_change))

        # loop until got valid answer from the user
        answer = ''
        while True:
            self.__show_summary(users_selections, cmd)

            print
            print info

            answer = raw_input("> ")

            if answer == answer_accept:
                # if removing - verify
                if cmd.is_remove:
                    sure = is_yes(sr.summary_txt_verify, False)

                    if sure:
                        return True

                    # if user is not sure (sure == False) then this loop will
                    # continue

                else:
                    return True


            elif answer == answer_change:
                return False

class QtUI(UI):
    '''The Qt user interface class'''

    def __init__(self):
        '''Constructor. 

        Raises FriendlyException if fails to initialize. The message of the
        raised exception will have the description of the problem.

        Raises CmdFatalFailure if package manager fails to install PyQt4
        bindings.

        [1] If DISPLAY not set UI cannot be started. Error message will be
        something like "cannot connect to X server". At least on suse DISPLAY
        is removed by sudo.
        '''

        try:
            if not os.environ.has_key('DISPLAY'): # see [1]
                raise FriendlyException("Environment variable DISPLAY is not set!")

            self.__create_wizard_classes()

        except AttributeError, e:
            MSGR.log("Traceback of handled exception")
            MSGR.log_exc()
            raise FriendlyException(
                "The version of Qt and PyQt bindings are too old. In order "
                "to use the GUI mode, please install Qt 4.3 or later and the "
                "respective python bindings.")

    def __create_wizard_classes(self):
        '''Creates Qt Wizard classes

        Can raise AttributeError if the version of PyQt4 is prior to Qt 4.3,
        since according to
        http://doc.trolltech.com/4.4/porting4-overview.html#wizard-dialogs-qwizard
        QWizard and siblings were added to that version of Qt4. Systems that
        have too old Qt and bindings by default: Debian Etch.'''

        QtGui = IMPORTER["PyQt4.QtGui"]
        QtCore = IMPORTER["PyQt4.QtCore"]

        class IntroPage(QtGui.QWizardPage):
            '''Introduction page'''

            def __init__(self, s):
                '''Constructor'''

                QtGui.QWizardPage.__init__(self)

                self.setTitle(s.intro_title)

                txt = s.intro_subtitle_gui

                # only root can install the Scratchbox
                if running_as_root() and \
                        PKG_MGR.repo_requires_authentication() and \
                        hasattr(s, "intro_creds_warn"): # only install has
                    txt += "<i>%s</i>" % \
                        s.intro_creds_warn()

                intro_label = QtGui.QLabel(txt)
                intro_label.setWordWrap(True)

                layout = QtGui.QVBoxLayout()
                layout.addWidget(intro_label)

                self.setLayout(layout)

        class LicensePage(QtGui.QWizardPage):
            '''EUSA page'''

            def __init__(self, s):
                '''Constructor'''

                QtGui.QWizardPage.__init__(self)

                self.setTitle(s.license_title)
                self.setSubTitle(s.license_subtitle)

                license_text_edit = QtGui.QTextEdit()
                license_text_edit.setPlainText(EUSA_TEXT)
                license_text_edit.setReadOnly(True)

                txt = "Nokia Binaries EUSA. Use Ctrl+Wheel to zoom the text."
                license_text_edit.setToolTip(txt)
                license_text_edit.setWhatsThis(txt)

                self.accept_check_box = QtGui.QCheckBox("I &accept the terms of the license")
                self.accept_check_box.setCheckState(QtCore.Qt.Unchecked)

                # will be emitted if license is accepted to enable next button
                self.connect(self.accept_check_box,
                             QtCore.SIGNAL("toggled(bool)"),
                             self,
                             QtCore.SIGNAL("completeChanged()"))

                layout = QtGui.QVBoxLayout()
                layout.addWidget(license_text_edit)
                layout.addWidget(self.accept_check_box)
                self.setLayout(layout)

            def isComplete(self):
                '''Overrides the method of QWizardPage, to disable next button if
                license is not accepted.'''
                return self.accept_check_box.isChecked()

        class UnsupThingPage(QtGui.QWizardPage):
            '''Page to show when something (e.g. some value of some kernel
            variable) is unsupported by either scratchbox, sdk or some other
            component. Normally supported setting will be set for the current
            boot session only. Hence, this page has a "permanency" checkbox,
            that if checked the supported setting should be set permanently.'''

            def __init__(self, text_title, text_subtitle, text_info,
                         text_perm, text_perm_hint, perm_attr):
                '''Constructor
                text_title = title text
                text_subtitle = subtitle text
                text_info, = text of the info label
                text_perm = text of the permanency checkbox
                text_perm_hint = hint of the permanency checkbox
                perm_attr = Name of the attribute of  wizards UsersSelection,
                            that will be set to permanency status'''

                QtGui.QWizardPage.__init__(self)

                self.perm_attr = perm_attr

                self.setTitle(text_title)
                self.setSubTitle(text_subtitle)

                info_label = QtGui.QLabel(text_info)
                info_label.setWordWrap(True)

                self.perm_checkbox = QtGui.QCheckBox(text_perm)

                txt = text_perm_hint
                self.perm_checkbox.setToolTip(txt)
                self.perm_checkbox.setWhatsThis(txt)

                layout = QtGui.QVBoxLayout()
                layout.addWidget(info_label)
                layout.addSpacing(40)
                layout.addWidget(self.perm_checkbox)
                self.setLayout(layout)

            def validatePage(self):
                '''Overrides the method of QWizardPage, to set the permanency
                attribute.'''
                setattr(self.wizard().users_selections,
                        self.perm_attr,
                        self.perm_checkbox.isChecked())

                return True

        class UsersPage(QtGui.QWizardPage):
            '''User selection page'''

            def __init__(self, s):
                '''Constructor'''
                QtGui.QWizardPage.__init__(self)

                self.setTitle(s.users_title)
                self.setSubTitle(s.users_subtitle)

                self.users_list_widget = QtGui.QListWidget()

                txt = s.users_list_hint
                self.users_list_widget.setToolTip(txt)
                self.users_list_widget.setWhatsThis(txt)

                # add users to the list
                for username in get_all_usernames():
                    item = QtGui.QListWidgetItem(username, self.users_list_widget)

                    item.setFlags(QtCore.Qt.ItemIsUserCheckable |
                                  QtCore.Qt.ItemIsEnabled)

                all_button = QtGui.QPushButton("&All")
                txt = "Selects all users"
                all_button.setToolTip(txt)
                all_button.setWhatsThis(txt)

                none_button = QtGui.QPushButton("N&one")
                txt = "Unselects all users"
                none_button.setToolTip(txt)
                none_button.setWhatsThis(txt)

                self.connect(none_button, QtCore.SIGNAL("clicked()"),
                             self.uncheckAllUsers)
                self.connect(all_button, QtCore.SIGNAL("clicked()"),
                             self.checkAllUsers)

                layout = QtGui.QGridLayout()
                layout.addWidget(self.users_list_widget, 0, 0, 1, 2)
                layout.addWidget(all_button, 1, 0, 1, 1)
                layout.addWidget(none_button, 1, 1, 1, 1)

                self.setLayout(layout)

            def initializePage(self):
                '''Overrides the method of QWizardPage, to initialize set the
                default selected users.'''
                self.checkUsers(self.wizard().users_selections.usernames)

            def validatePage(self):
                '''Overrides the method of QWizardPage, to read the selected
                users'''
                usernames = set()

                # loop through items in the list and store selected ones
                for i in range(self.users_list_widget.count()):
                    item = self.users_list_widget.item(i)

                    if item.checkState() == QtCore.Qt.Checked:
                        usernames.add(str(item.text()))

                self.wizard().users_selections.usernames = usernames

                return True

            def allUsersSetCheckState(self, state):
                '''Sets the CheckState of all users in the list widget as
                specified'''
                for i in range(self.users_list_widget.count()):
                    item = self.users_list_widget.item(i)
                    item.setCheckState(state)

            def checkAllUsers(self):
                '''Selects all users'''
                self.allUsersSetCheckState(QtCore.Qt.Checked)

            def uncheckAllUsers(self):
                '''Unselects all users'''
                self.allUsersSetCheckState(QtCore.Qt.Unchecked)

            def checkUsers(self, usernames):
                '''Checks users from the usernames list, unchecks all others'''
                for i in range(self.users_list_widget.count()):
                    item = self.users_list_widget.item(i)

                    if str(item.text()) in usernames:
                        item.setCheckState(QtCore.Qt.Checked)
                    else:
                        item.setCheckState(QtCore.Qt.Unchecked)

        class TargetsPage(QtGui.QWizardPage):
            '''Targets page, shown only if targets exist for selected
            user. Thus shall be shown after the users page.'''

            def __init__(self, s):
                '''Constructor'''
                QtGui.QWizardPage.__init__(self)

                self.s = s

                self.setTitle(s.targets_title)
                # subtitle is set in initializePage

                self.remove_targets_button = \
                    QtGui.QRadioButton(s.targets_txt_overwrite_acc)
                self.create_targets_button = \
                    QtGui.QRadioButton(s.targets_txt_create_acc)

                self.button_group = QtGui.QButtonGroup()
                self.button_group.addButton(self.remove_targets_button, 0)
                self.button_group.addButton(self.create_targets_button, 1)

                # targets name prefix group box
                self.group_box = QtGui.QGroupBox(s.targets_create_title_acc)
                #self.group_box.setFlat(True)

                info_str = s.targets_create_subtitle

                info_label = QtGui.QLabel(info_str)
                info_label.setWordWrap(True)

                self.target_prefix_line_edit = QtGui.QLineEdit()

                group_box_layout = QtGui.QVBoxLayout()
                group_box_layout.addStretch()
                group_box_layout.addWidget(info_label)
                group_box_layout.addWidget(self.target_prefix_line_edit)

                self.group_box.setLayout(group_box_layout)

                self.connect(self.remove_targets_button,
                             QtCore.SIGNAL("toggled(bool)"),
                             self.removeTargetsButtonToggled)

                self.create_targets_button.setChecked(True)

                layout = QtGui.QVBoxLayout()
                layout.addWidget(self.remove_targets_button)
                layout.addWidget(self.create_targets_button)
                layout.addStretch()
                layout.addWidget(self.group_box)
                self.setLayout(layout)

            def initializePage(self):
                '''Overrides the method of QWizardPage, to initialize the page
                with some dynamic text.'''

                targets_exist = self.wizard().users_selections.targets_exist
                usernames = self.wizard().users_selections.usernames

                # list of usernames who already has targets
                usernames_ex = \
                    self.wizard().users_selections.existing_targets.keys()

                self.setSubTitle(self.s.targets_subtitle(usernames_ex))

                # set the line edits text to some non-existing target prefix
                nonexistent_prefix = sb_get_nonexistent_prefix(usernames)
                self.target_prefix_line_edit.setText(nonexistent_prefix)

            def validatePage(self):
                '''Overrides the method of QWizardPage, verify valid input from
                the user'''
                targets_remove = self.remove_targets_button.isChecked()

                # not removing targets, verify that prefix is not used in sb
                if not targets_remove:
                    targets_prefix = self.target_prefix_line_edit.text()
                    usernames = self.wizard().users_selections.usernames

                    # prefix exist: show error box and prevent leaving page
                    if sb_prefix_exists_any(usernames, targets_prefix):

                        usernames_exists = \
                            sb_get_prefix_exists_usernames(usernames,
                                                           targets_prefix)

                        usernames_str = seq_to_str(usernames_exists, ", ", "")

                        txt = self.s.targets_exists_txt % (targets_prefix,
                                                           usernames_str)

                        QtGui.QMessageBox.critical(
                            self,
                            self.s.targets_exists_title, 
                            txt)
                        return False

                    # prefix does not exist in sb for any user, store it
                    else:
                        self.wizard().users_selections.targets_prefix = \
                            targets_prefix

                # leaving targets page
                self.wizard().users_selections.targets_remove = targets_remove

                return True

            def removeTargetsButtonToggled(self, on):
                '''Handler for toggled signal of targets remove radio
                button. Enables target name prefix group box if checkbox is
                unchecked, and disables the group box otherwise. When the group
                box is enabled the focus is set to line edit'''
                self.group_box.setDisabled(on)

                if not on: # group box is enabled
                    self.target_prefix_line_edit.setFocus()

        class SummaryPage(QtGui.QWizardPage):
            '''Summary page'''

            def __init__(self, s):
                '''Constructor'''

                QtGui.QWizardPage.__init__(self)

                self.s = s

                self.setTitle(s.summary_title)
                self.setSubTitle(s.summary_subtitle)

                # pages after this will have back button disabled
                self.setCommitPage(True)

                self.summary_label = QtGui.QLabel()
                self.summary_label.setWordWrap(True)
                self.summary_label.setAlignment(QtCore.Qt.AlignLeft |
                                                QtCore.Qt.AlignTop)

                # scroll area is used to add a scrolling capability in case the
                # text in the label becomes too long
                scroll_area = QtGui.QScrollArea(self)
                scroll_area.setWidgetResizable(True)
                scroll_area.setFrameStyle(QtGui.QFrame.NoFrame)
                scroll_area.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAsNeeded)
                scroll_area.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAsNeeded)

                # widget placed in scroll area
                sa_widget = QtGui.QWidget()

                # layout for the widget
                sa_widget_layout = QtGui.QVBoxLayout(sa_widget)
                sa_widget_layout.addWidget(self.summary_label)

                scroll_area.setWidget(sa_widget)

                # layout for the page
                layout = QtGui.QVBoxLayout()
                layout.addWidget(scroll_area)
                self.setLayout(layout)

            def initializePage(self):
                '''Overrides the method of QWizardPage, to initialize the page
                with summary text.'''

                self.wizard().setButtonText(QtGui.QWizard.CommitButton, 
                                            "&%s %s" %
                                            (self.s.name_verb_simple_present.capitalize(),
                                             PRODUCT_NAME_SHORT))

                summary = self.wizard().users_selections.get_summary_text(
                    '<br>', '<b>', '</b>')
                self.summary_label.setText(summary)

            def validatePage(self):
                '''Overrides the method of QWizardPage'''
                # if removing - verify
                if self.wizard().cmd.is_remove:
                    reply = QtGui.QMessageBox.warning(self,
                                                      "Files will be removed!",
                                                      sr.summary_txt_verify,
                                                      QtGui.QMessageBox.Yes |
                                                      QtGui.QMessageBox.No,
                                                      QtGui.QMessageBox.No)

                    if reply == QtGui.QMessageBox.No:
                        return False

                return True

        class ThreadSafeAnswer(object):
            '''Thread-safe answer to the questions asked in the GUI.  Used to
            pass the answer to the question asked in the main (GUI) thread to
            the executor thread.'''

            def __init__(self):
                '''Constructor'''

                self.__null_answer = None
                self.__answer = self.__null_answer

                # mutex protects access to answer
                self.__mutex = QtCore.QMutex()

                self.__answerSet = QtCore.QWaitCondition()

            def set(self, answer):
                '''Sets the answer'''
                locker = QtCore.QMutexLocker(self.__mutex)
                self.__answer = answer
                self.__answerSet.wakeAll()

            def __get(self):
                '''Returns the answer'''
                locker = QtCore.QMutexLocker(self.__mutex)

                if self.__answer == self.__null_answer:
                    self.__answerSet.wait(self.__mutex)

                return self.__answer

            def getAndNull(self):
                '''Reads the answer and sets it to null. Returns the answer.'''
                answer = self.__get()
                self.set(self.__null_answer)
                return answer

        class ExecutorThread(QtCore.QThread):
            '''Thread that executes the installation or removal functions.
            This is not done in the GUI thread not to freeze the GUI.

            Because in GUI applications, the main thread (a.k.a. the GUI
            thread) is the only thread that is allowed to perform GUI-related
            operations, signals are sent from this thread to the main thread to
            say or ask something from the user.'''

            def __init__(self, ts_answer, cmd, *args, **kwds):
                '''Constructor
                ts_answer = answer to the asked question from the GUI thread
                cmd = Command to run in the thread
                args = Arguments to pass to run method of the command
                kwds = Keyword arguments to pass to run method of the command
                '''
                QtCore.QThread.__init__(self)
                self.ts_answer = ts_answer
                self.cmd = cmd
                self.args = args
                self.kwds = kwds

                # signal emitted when func asks something
                # short-circuit signal another option is PyQt_PyObject
                self.sig_ask = QtCore.SIGNAL("ask")

                self.sig_phase_started = QtCore.SIGNAL("phaseStarted(const QString&)")
                self.sig_task_started = QtCore.SIGNAL("taskStarted(const QString&)")
                self.sig_tasks_range_changed = QtCore.SIGNAL("tasksRangeChanged(int, int)")
                self.sig_tasks_done_num_changed = QtCore.SIGNAL("tasksDoneNumChanged(int)")

                # abort installation after current task is completed
                self.__abort = False

                # tasks done_num as passed to say is going from 1 to
                # total_num. done_num is decremented in the emitted
                # sig_tasks_done_num_changed to keep the progress bar in
                # shape. This approach has the problem that last done_num is
                # not passed to the emitted signal, so progress bar remains
                # incomplete by one step.  Hence, this variable is set to
                # done_num of the last task and passed on via signal on the
                # next say.
                self.__last_done_num = 0

            def run(self):
                '''Runs the installation or removal function'''
                MSGR.set_override_say(self.say)
                MSGR.set_override_ask(self.ask)
                args = self.args + (self.isAborting,)
                self.cmd.run(*args, **self.kwds)
                MSGR.remove_override_ask()
                MSGR.remove_override_say()

            def say(self, msg, say_type, done_num, total_num):
                '''Overrides the default say functionality of the Messenger, to
                emit signals so that GUI thread would update the UI with the
                new info.'''

                if self.__last_done_num:
                    self.emit(self.sig_tasks_done_num_changed, self.__last_done_num)
                    self.__last_done_num = 0

                # process tasks and normal messages
                if say_type == Messenger.SayTypeTask:

                    # tasks and the rest are printed with this
                    self.emit(self.sig_task_started, msg)

                    # new batch of tasks started
                    if done_num == Messenger.DoneNumStart:
                        self.emit(self.sig_tasks_range_changed, 0, total_num)

                    # next task started
                    if done_num != Messenger.DoneNumNotUsed:
                        self.emit(self.sig_tasks_done_num_changed, done_num - 1)

                        if done_num == total_num: # last task
                            self.__last_done_num = done_num

                # phases
                elif say_type == Messenger.SayTypePhase:
                    self.emit(self.sig_phase_started, msg)

            def ask(self, title, question, choices, default_choice):
                '''Overrides the default ask functionality of the Messenger.
                Emits a signal so that the GUI thread could ask the question,
                then reads the answer from the GUI thread.'''
                self.emit(self.sig_ask, title, question, choices, default_choice)
                answer = self.ts_answer.getAndNull()

                return answer

            def isRunningLastTask(self):
                '''Whether this thread is executing last task.'''
                return self.cmd.is_running_last_task

            def abort(self):
                '''Called by the main thread to stop this thread after current
                installation operation completes. If the thread is running last
                task, the request to abort will be ignored, since the
                installation will end after that task anyway.

                It is possible to have the abort dialog shown in the beginning
                while the executor will proceed to run last task, in that case
                the user will have "Abort later" button visible even though
                last task is executed.'''

                if not self.isRunningLastTask():
                    MSGR.log("Executor accepted request to abort")
                    self.__abort = True
                else:
                    MSGR.log("Executor ignoring abort request since running last task")

            def isAborting(self):
                '''Whether this thread is about to abort the installation.'''
                return self.__abort

        class ProgressPage(QtGui.QWizardPage):
            '''Progress page'''

            def __init__(self, s):
                '''Constructor'''

                QtGui.QWizardPage.__init__(self)

                self.s = s
                self.ts_answer = ThreadSafeAnswer()

                # whether the slider in the text widget has been scrolled to
                # its vertical scroll bars end, it will be scrolled only once
                # at initialization; when the slider is scrolled to the
                # vertical end, the newly appended text will cause automatic
                # scrolling, so the new text will always be visible
                self.scrolled_to_vend = False

                self.setTitle(s.progress_title)
                self.setSubTitle(s.progress_subtitle)

                self.status_label = QtGui.QLabel("%s..." % (s.name_verb_present_continuous.capitalize()))
                self.status_label.setWordWrap(True)

                self.progress_bar = QtGui.QProgressBar()

                txt = ('Log file <a href="file://%s">%s</a>' %
                       (MSGR.fn_log, MSGR.fn_log))

                self.logs_url_label = QtGui.QLabel(txt)
                self.logs_url_label.setWordWrap(True)
                self.logs_url_label.setOpenExternalLinks(True)
                self.logs_url_label.setTextInteractionFlags(
                    QtCore.Qt.TextBrowserInteraction)

                # some systems have such bug that file can't be opened by
                # clicking on URL, so no hint on using the link, see
                # https://bugs.launchpad.net/ubuntu/+source/xdg-utils/+bug/362121

                # txt = "Use link to open log file"
                # logs_url_label.setToolTip(txt)
                # logs_url_label.setWhatsThis(txt)

                self.logs_text_edit = QtGui.QTextEdit()

                txt = "Logs from %s.  Use Ctrl+Wheel to zoom the text." % MSGR.fn_log
                self.logs_text_edit.setToolTip(txt)
                self.logs_text_edit.setWhatsThis(txt)

                # set proper coloring of the text edit
                # must be done before writing any text, otherwise black text
                # will not be visible once you change background to black
                #
                # new in Qt 4.4: not used now, since some systems won't have it
                # self.logs_text_edit.setTextBackgroundColor(QtCore.Qt.black)
                palette = QtGui.QPalette()
                palette.setColor(QtGui.QPalette.Active, QtGui.QPalette.Base, QtCore.Qt.black);
                palette.setColor(QtGui.QPalette.Inactive, QtGui.QPalette.Base, QtCore.Qt.black);
                self.logs_text_edit.setPalette(palette);
                self.logs_text_edit.setTextColor(QtCore.Qt.green)

                # there must be some text in the editor, otherwise the color of
                # text written with cursor will be black, why? go figure...
                self.logs_text_edit.setPlainText("logs from %s\n" % MSGR.fn_log)

                self.logs_text_edit.setReadOnly(True)

                # to look like a proper terminal use fixed width fonts
                self.logs_text_edit.setFont(QtGui.QFont("Monospace", 10))

                # cursor is used to append text to the end of the edit widget,
                # there is append method but it adds extra newline, and
                # InsertPlainText method inserts text at current cursor
                # position, which can be changed by the user just by clicking
                # somewhere in the edit widget
                self.cursor = self.logs_text_edit.textCursor()
                self.cursor.movePosition(QtGui.QTextCursor.End)

                self.logs_button = QtGui.QPushButton("&Logs")
                txt = "Toggles visibility of the logs view"
                self.logs_button.setToolTip(txt)
                self.logs_button.setWhatsThis(txt)
                self.logs_button.setCheckable(True)
                self.connect(self.logs_button,
                             QtCore.SIGNAL("toggled(bool)"),
                             self.logsButtonToggled)

                # takes the space of text edit when it is hidden
                self.spacer = QtGui.QSpacerItem(0, 0,
                                                QtGui.QSizePolicy.Minimum,
                                                QtGui.QSizePolicy.Expanding)

                # process that tails the log file
                self.tail_process = QtCore.QProcess(self)
                self.tail_process.start("tail -f " + MSGR.fn_log)
                self.connect(self.tail_process,
                             QtCore.SIGNAL("readyReadStandardOutput()"),
                             self.tailProcessReadStdout)

                layout = QtGui.QVBoxLayout()
                layout.addWidget(self.status_label)
                layout.addWidget(self.progress_bar)
                # spacer will be removed in button's toggled signal handler
                layout.addItem(self.spacer)
                layout.addWidget(self.logs_url_label)
                layout.addWidget(self.logs_text_edit)
                self.setLayout(layout)

                self.logs_button.setChecked(True) # will emit toggled signal

            def __del__(self):
                '''Destructor. Kills the tail process, otherwise it will
                outlive the setup (although Qt documentation says the
                destructor of QProcess should kill the process)'''

                if hasattr(self, "tail_process"):
                    self.tail_process.kill()

            def initializePage(self):
                '''Overrides the method of QWizardPage, to initialize the page
                with some default settings.'''

                QtGui.QApplication.setOverrideCursor(QtCore.Qt.BusyCursor)

                self.executor = ExecutorThread(self.ts_answer,
                                               self.wizard().cmd,
                                               self.wizard().users_selections)

                self.connect(self.executor, self.executor.sig_task_started,
                             self.status_label,
                             QtCore.SLOT("setText(const QString&)"))

                self.connect(self.executor, self.executor.sig_phase_started,
                             self.setSubTitle)

                self.connect(self.executor, self.executor.sig_tasks_range_changed,
                             self.progress_bar,
                             QtCore.SLOT("setRange(int, int)"))

                self.connect(self.executor, self.executor.sig_tasks_done_num_changed,
                             self.progress_bar,
                             QtCore.SLOT("setValue(int)"))

                self.connect(self.executor, self.executor.sig_ask, self.ask)

                self.connect(self.executor, QtCore.SIGNAL("finished()"),
                             self.executorFinished)

                self.executor.start()

                # have a custom button to show/hide logs
                self.wizard().setOption(QtGui.QWizard.HaveCustomButton1, True)
                self.wizard().setButton(QtGui.QWizard.CustomButton1, self.logs_button)

            def validatePage(self):
                '''Overrides the method of QWizardPage, to remove the custom
                button.'''
                self.wizard().setOption(QtGui.QWizard.HaveCustomButton1, False)

                # QWizard deletes the old button when setButton is used
                self.wizard().setButton(QtGui.QWizard.CustomButton1, None)

                return True

            def isBusy(self):
                '''Returns True if installation/removal is in progress'''
                return self.executor.isRunning()

            def executorFinished(self):
                '''Called when executor thread has finished its jobs. This
                means installation or removal is complete at this point. So
                this routine reflects that to the UI'''
                status = self.wizard().cmd.status

                QtGui.QApplication.restoreOverrideCursor()

                if hasattr(QtGui.QApplication, "alert"):# available since Qt4.3
                    QtGui.QApplication.alert(self.wizard())

                name_noun_cap = self.s.name_noun.capitalize()
                self.setTitle("%s process completed" % (name_noun_cap,))
                self.setSubTitle(sc.summary(self.s, status))


                # fatal error
                if status == Command.StatusFatal:

                    # show the failed op the label
                    self.status_label.setText(
                        "<font color=red><b>Failed: %s</font></b>" %
                        self.status_label.text())

                    # message box with fatal error
                    QtGui.QMessageBox.critical(
                        self, "Fatal Error",
                        "%s\n%s\n%s" % (sc.fatal_title, MSGR.get_fatal(),
                                        sc.contact_info_on_problems()))

                # non-critical errors
                elif status == Command.StatusErrors:
                    self.status_label.setText("Some non-critical tasks failed")
                    
                    txt = sc.errors_title + '\n'

                    for err in MSGR.get_errors():
                        txt += err + "\n"

                    txt += sc.contact_info_on_problems()

                    QtGui.QMessageBox.warning(self, "Non-critical Errors", txt)

                # abort
                elif status == Command.StatusAborted:
                    self.status_label.setText("Aborted")

                # ok
                elif status == Command.StatusCompleted:
                    self.status_label.setText("All tasks done")

                else:
                    assert False, "Illegal command exit status (%s)!" % \
                        (status)

                # enable the Next/Finish button
                self.emit(QtCore.SIGNAL("completeChanged()"))

                # Finish button will replace Next button if installation failed
                # setFinalPage works with Qt versions 4.5 and higher, see the
                # bug:
                # http://www.qtsoftware.com/developer/task-tracker/index_html?method=entry&id=222140
                if self.isLastPage():
                    if QtCore.QT_VERSION >= 0x040500:
                        self.setFinalPage(True)

                    else:
                        finish_btn = self.wizard().button(QtGui.QWizard.FinishButton)
                        finish_btn.setVisible(True)
                        finish_btn.setEnabled(True)
                        finish_btn.setDefault(True)

                        next_btn = self.wizard().button(QtGui.QWizard.NextButton)
                        next_btn.setVisible(False)

                # from this page on there is nothing to cancel, so disabled
                self.wizard().button(QtGui.QWizard.CancelButton).setEnabled(False)

            def isLastPage(self):
                '''Returns True if this page is the last one to show (in which
                case Finish button will replace the Next button). This page
                will be the last if installation fails.'''
                status = self.wizard().cmd.status

                return status in [Command.StatusAborted, Command.StatusFatal]

            def nextId(self):
                '''Overrides the method of QWizardPage, not to show last page
                in case installation fails.'''
                # installation failed
                if self.isLastPage():
                    return -1

                # installation succeeded
                else:
                    return QtGui.QWizardPage.nextId(self)

            def isComplete(self):
                '''Overrides the method of QWizardPage, to disable next button
                when installation/removal is in progress.'''
                if self.isBusy():
                    return False
                else:
                    return True

            def showAbortMsgBox(self):
                '''Shows abort message box. Returns tuple of buttons texts.'''

                name_noun = self.s.name_noun

                msg_box = QtGui.QMessageBox(
                    QtGui.QMessageBox.Question,
                    "Really abort?",
                    "%s processes are still running!" % \
                        name_noun.capitalize())

                # have not chosen to abort already in the past and not running
                # last task
                have_abort_later_btn = not self.executor.isAborting() and \
                    not self.executor.isRunningLastTask()

                abort_now_btn_txt = "Abort now"
                abort_later_btn_txt = "Abort later"

                abort_now_info_text = (
                    "If you choose to '%s' all of the %s processes will be "
                    "killed. <b>NOTE!</b> <i>This could potentially make "
                    "your system unstable. So use it at your own risk.</i>" %
                    (abort_now_btn_txt, name_noun))

                if have_abort_later_btn:
                    abort_later_info_text = (
                        "You can abort safely by choosing '%s', in which "
                        "case currently running %s process will be allowed "
                        "to complete its execution." %
                        (abort_later_btn_txt, name_noun))

                # don't have the abort later button
                else:
                    if self.executor.isRunningLastTask():
                        abort_later_info_text = (
                            "It appears that the last %(name_noun)s process "
                            "is running. Hence, %(name_noun)s should be over "
                            "any minute now! It is highly recommended to wait "
                            "for the completion of %(name_noun)s instead of "
                            "aborting." % { "name_noun" : name_noun })

                    else:
                        abort_later_info_text = (
                            "You have previously selected '%(btn_txt)s', so "
                            "%(name_noun)s will be aborted after currently "
                            "running %(name_noun)s process completes its "
                            "execution." % { "btn_txt" : abort_later_btn_txt,
                                             "name_noun" : name_noun })

                info_txt = (abort_later_info_text + "<br><br>" +
                            abort_now_info_text)

                msg_box.setInformativeText(info_txt)

                cancel_btn = msg_box.addButton(QtGui.QMessageBox.Cancel)
                abort_now_btn = \
                    msg_box.addButton(abort_now_btn_txt,
                                      QtGui.QMessageBox.DestructiveRole)

                if have_abort_later_btn:
                    abort_later_btn = \
                        msg_box.addButton(abort_later_btn_txt,
                                          QtGui.QMessageBox.AcceptRole)

                msg_box.setDefaultButton(cancel_btn)

                msg_box.exec_()

                clicked_btn = msg_box.clickedButton()
                MSGR.log("Answer to abort dialog: %s" % clicked_btn.text())

                # msg_box & clicked_btn will not exist after this function
                # returns
                return (clicked_btn.text(), abort_now_btn_txt,
                        abort_later_btn_txt, cancel_btn.text())

            def onCancel(self):
                '''Called if user tries to close or cancel the wizard, shows the abort
                dialog and takes respective actions.'''
                (clicked_btn_txt, abort_now_btn_txt,
                 abort_later_btn_txt, cancel_btn_txt) = self.showAbortMsgBox()

                # cannot abort if installation has already ended
                if clicked_btn_txt != cancel_btn_txt:
                    if not self.isBusy():
                        QtGui.QMessageBox.information(
                            self,
                            "Sorry!",
                            "Cannot '%s' since the %s has ended!" % \
                                (clicked_btn_txt, self.s.name_noun),
                            QtGui.QMessageBox.Ok)
                        return

                # abort later
                if clicked_btn_txt == abort_later_btn_txt:
                    if self.executor.isRunningLastTask():
                        QtGui.QMessageBox.information(
                            self,
                            "Sorry!",
                            "Cannot '%s' since the last %s process is running!"
                            % (abort_later_btn_txt, self.s.name_noun),
                            QtGui.QMessageBox.Ok)
                    else:
                        self.executor.abort()

                # abort now: kill self & children too
                elif clicked_btn_txt == abort_now_btn_txt:
                    kill_all()

            def showEvent(self, event):
                '''Overriden method of QWidget. Scrolls the slider of the
                vertical scroll bar of the text edit to the end.  This is done
                to enable automatic scrolling of the scrollbar upon addition
                of the new text.  Done only once, when the page is shown for
                the first time.'''
                QtGui.QWizardPage.showEvent(self, event)

                if not self.scrolled_to_vend:
                    self.logsTextEditScrollToVend()
                    self.scrolled_to_vend = True

            def logsButtonToggled(self, checked):
                '''Slot for the toggled signal of the logs button. Hides/shows
                logs text edit.'''
                self.logs_text_edit.setVisible(checked)
                self.logs_url_label.setVisible(checked)

                if checked:
                    # when text edit is visible spacer is removed, otherwise
                    # spacer would visibly consume space if page is resized
                    self.layout().removeItem(self.spacer)

                else:
                    # when text edit is hidden the spacer is used to take its
                    # space to prevent other widget from moving in the layout
                    self.layout().insertItem(2, self.spacer)

            def logsTextEditScrollToVend(self):
                '''Scrolls vertical scroll bar of the text edit widget to
                the end'''
                vsb = self.logs_text_edit.verticalScrollBar()
                vsb.setValue(vsb.maximum())

            def logsTextEditRemoveLastLine(self):
                '''Removes last line from the text edit'''
                cursor = self.logs_text_edit.textCursor()
                cursor.movePosition(QtGui.QTextCursor.End)
                cursor.select(QtGui.QTextCursor.BlockUnderCursor)
                cursor.deletePreviousChar()

            def splitStringWithMultiCR(self, txt):
                '''Splits a string containing multiple lines of text into list
                a of single-line strings. Character \r is assumed to mark the
                beginning of a line, whereas character \n is assumed to mark
                the end of a line. This routine is used to emulate terminal
                carriage return (\r) handling.

                returns a list of strings
                txt = a string containing multiple lines of text (including \n,
                      \r etc)'''
                txt_list = []

                i_begin = 0 # index, where the next line should begin from

                for i in xrange(0, len(txt)):

                    # \r is assumed to mark the beginning of a new line
                    if txt[i] == '\r':

                        # there could be \r just after line ending with \n or
                        # the whole text could begin with \r: we don't want
                        # empty string in those cases
                        if i - i_begin > 0:
                            txt_list.append(txt[i_begin:i])
                            i_begin = i

                    # \n is assumed to mark the end of a current line, next
                    # char after newline will be the start of the next line
                    elif txt[i] == '\n':
                        txt_list.append(txt[i_begin:i + 1])
                        i_begin = i + 1

                # if text does not end with \n, get the last line
                if i_begin < len(txt):
                    txt_list.append(txt[i_begin:len(txt)])

                return txt_list

            def logsTextEditAppend(self, txt):
                '''Appends text into the text edit widget. If there are
                carriage return characters in the text does some processing in
                order to emulate terminal behavior.

                txt = a string containing multiple lines of text (including \n,
                      \r etc)'''
                vsb = self.logs_text_edit.verticalScrollBar()

                at_bottom = vsb.value() == vsb.maximum()

                # \r\n is just newline (some apps use it, e.g. apt/dpkg)
                txt = txt.replace("\r\n", "\n")

                # number of carriage return characters in the text
                cr_count = txt.count('\r')

                # text without carriage return is just inserted
                if cr_count == 0:
                    self.cursor.insertText(txt)

                # text has only one carriage return in the beginning, previous
                # line must be removed before the text is inserted
                elif cr_count == 1 and txt.startswith('\r'):
                    self.logsTextEditRemoveLastLine() 
                    self.cursor.insertText(txt)

                # text has multiple carriage return characters
                else:
                    txt_list = self.splitStringWithMultiCR(txt)
                    for line in txt_list:
                        if line.startswith('\r'):
                            self.logsTextEditRemoveLastLine()
                        self.cursor.insertText(line)

                # automatically scroll, if slider was at the end of scrollbar
                if at_bottom:
                    self.logsTextEditScrollToVend()

            def tailProcessReadStdout(self):
                '''A slot that is called when the tail process has some text on
                its stdout. Appends that text to the text edit.'''
                txt = str(self.tail_process.readAllStandardOutput())
                self.logsTextEditAppend(txt)

            def ask(self, title, question, choices, default_choice):
                '''Asks question from the user'''

                # expand on demand
                choice_to_button = { ASK_CHOICE_RETRY : QtGui.QMessageBox.Retry,
                                     ASK_CHOICE_ABORT : QtGui.QMessageBox.Abort }

                # find out the buttons
                buttons = 0
                for i in choices:
                    buttons |= choice_to_button[i]

                default_button = choice_to_button[default_choice]

                reply = QtGui.QMessageBox.warning(self, title, question,
                                                  buttons, default_button)

                # get the choice from button
                for k, v in choice_to_button.items():
                    if v == reply:
                        result = k
                        break

                self.ts_answer.set(result)

        class ConclusionPage(QtGui.QWizardPage):
            '''ConclusionPage page, shown only on successful completion of the
            command.'''

            def __init__(self, s):
                '''Constructor'''

                QtGui.QWizardPage.__init__(self)

                self.setTitle(s.conclusion_title)

                label = QtGui.QLabel(s.conclusion_subtitle)
                label.setWordWrap(True)

                layout = QtGui.QVBoxLayout()
                layout.addWidget(label)
                self.setLayout(layout)

        PageIdIntro      = 0
        PageIdLicense    = 1
        PageIdVDSO       = 2
        PageIdSELinux    = 3
        PageIdMMapMinA   = 4
        PageIdUsers      = 5
        PageIdTargets    = 6 # used only by install wizard
        PageIdSummary    = 7
        PageIdProgress   = 8
        PageIdConclusion = 9

        class WizardBase(QtGui.QWizard):
            '''Base class for wizards, contains all common functionality.'''

            def __init__(self, cmd):
                '''Constructor'''

                QtGui.QWizard.__init__(self)

                self.cmd = cmd

                self.users_selections = None
                self.s = None # class of strings

                self.setWindowTitle("%s (%s)" % (MY_NAME, self.cmd.name))

                # self.setOption(QtGui.QWizard.NoBackButtonOnStartPage)
                # self.setOption(QtGui.QWizard.NoBackButtonOnLastPage)
                self.setOption(QtGui.QWizard.DisabledBackButtonOnLastPage)

            def reject(self):
                '''Overridden method of QDialog to disable wizard closing when
                installation/removal is in progress. Handles closing wizard by:
                - pressing Esc button
                - clicking Cancel button
                - clicking X button on the title bar of the window
                - any shortcut that can be used to close a window
                - probably any other means used to close a window'''

                if self.currentId() == PageIdProgress and \
                        self.currentPage().isBusy():

                    self.currentPage().onCancel()
                    return

                # default behavior in other cases
                QtGui.QWizard.reject(self)

            def __del__(self):
                '''Destructor. Calls destructors of pages, since PyQt does not
                do that.'''
                MSGR.log("Destroying the wizard")

                # pageIds was introduced in Qt 4.5.
                if hasattr(QtGui.QWizard, "pageIds"):
                    ids = self.pageIds()

                # roughly does the same
                else:
                    id_min = PageIdIntro
                    id_max = PageIdConclusion
                    ids = []

                    for id in range(id_min, id_max + 1):
                        page = self.page(id)
                        if page:
                            ids.append(id)

                # call the pages destructor if exists
                for id in ids:
                    page = self.page(id)
                    if hasattr(page, "__del__"):
                        page.__del__()
                        MSGR.log("Calling destructor of %s" % page)

        class AdminInstallWizard(WizardBase):
            '''Admin installation wizard'''

            def __init__(self, cmd):
                '''Constructor'''

                WizardBase.__init__(self, cmd)

                self.users_selections = InstallSelections()

                self.setPage(PageIdIntro, IntroPage(cmd.s))
                self.setPage(PageIdLicense, LicensePage(cmd.s))

                if SYS_INFO.has_unsup_vdso:
                    self.setPage(PageIdVDSO,
                                 UnsupThingPage(cmd.s.vdso_title,
                                                cmd.s.vdso_subtitle,
                                                cmd.s.vdso_info_gui,
                                                cmd.s.vdso_perm_text_acc,
                                                cmd.s.vdso_perm_hint,
                                                "set_perm_vdso"))

                if SYS_INFO.has_unsup_selinux:
                    self.setPage(PageIdSELinux,
                                 UnsupThingPage(cmd.s.selinux_title,
                                                cmd.s.selinux_subtitle,
                                                cmd.s.selinux_info_gui,
                                                cmd.s.selinux_perm_text_acc,
                                                cmd.s.selinux_perm_hint,
                                                "set_perm_selinux"))

                if SYS_INFO.has_unsup_mmap_mina:
                    self.setPage(PageIdMMapMinA,
                                 UnsupThingPage(cmd.s.mmap_mina_title,
                                                cmd.s.mmap_mina_subtitle,
                                                cmd.s.mmap_mina_info_gui,
                                                cmd.s.mmap_mina_perm_text_acc,
                                                cmd.s.mmap_mina_perm_hint,
                                                "set_perm_mmap_mina"))

                self.setPage(PageIdUsers, UsersPage(cmd.s))
                self.setPage(PageIdTargets, TargetsPage(cmd.s))
                self.setPage(PageIdSummary, SummaryPage(cmd.s))
                self.setPage(PageIdProgress, ProgressPage(cmd.s))
                self.setPage(PageIdConclusion, ConclusionPage(cmd.s))

            def nextId(self):
                '''Returns ID of page to show when the user clicks the Next
                button.'''
                if self.currentId() == PageIdUsers:
                    if self.users_selections.targets_exist:
                        return PageIdTargets
                    else:
                        return PageIdSummary

                # default behavior for other pages
                return WizardBase.nextId(self)

        class UserInstallWizard(WizardBase):
            '''User installation wizard'''

            def __init__(self, cmd):
                '''Constructor'''

                WizardBase.__init__(self, cmd)

                self.users_selections = InstallSelections()

                self.setPage(PageIdIntro, IntroPage(cmd.s))
                self.setPage(PageIdLicense, LicensePage(cmd.s))
                self.setPage(PageIdTargets, TargetsPage(cmd.s))
                self.setPage(PageIdSummary, SummaryPage(cmd.s))
                self.setPage(PageIdProgress, ProgressPage(cmd.s))
                self.setPage(PageIdConclusion, ConclusionPage(cmd.s))

            def nextId(self):
                '''Returns ID of page to show when the user clicks the Next
                button.'''
                if self.currentId() == PageIdLicense:
                    if self.users_selections.targets_exist:
                        return PageIdTargets
                    else:
                        return PageIdSummary

                # default behavior for other pages
                return WizardBase.nextId(self)

        class AdminRemoveWizard(WizardBase):
            '''Admin removal wizard'''

            def __init__(self, cmd):
                '''Constructor'''

                WizardBase.__init__(self, cmd)

                self.users_selections = RemoveSelections()

                self.setPage(PageIdIntro, IntroPage(cmd.s))
                self.setPage(PageIdSummary, SummaryPage(cmd.s))
                self.setPage(PageIdProgress, ProgressPage(cmd.s))
                self.setPage(PageIdConclusion, ConclusionPage(cmd.s))

        self.wizard_classes = { AdminInstallCommand : AdminInstallWizard,
                                UserInstallCommand : UserInstallWizard,
                                AdminRemoveCommand : AdminRemoveWizard }

    def run_cmd(self, cmd):
        '''See docstring of the same method of the parent class'''

        QtGui = IMPORTER["PyQt4.QtGui"]

        app = QtGui.QApplication([])

        wizard = self.wizard_classes[cmd.__class__](cmd)

        wizard.show()
        app.exec_()

def create_ui():
    '''UI factory function. Creates suitable UI object and returns it.
    Will return None in some error cases'''

    ui = None
    fallback_to_cmd_line_ui = False
    user_chose_qt_ui = OPTIONS.interface == CHOICE_UI_QT 
    

    # create Qt GUI
    if user_chose_qt_ui:
        try:
            ui = QtUI()

        # failed to create Qt GUI
        except Exception, e:
            print_wrapped(str(e))
            MSGR.log_exc()

            print "\nFailed to start-up Qt GUI, for details see %s" % \
                (MSGR.fn_log,)

            fallback_to_cmd_line_ui = \
                is_yes("Fall back to command line interface?")

    # create command line UI
    if not user_chose_qt_ui or fallback_to_cmd_line_ui:
        ui = CmdLineUI()

    return ui

def get_user_conf():
    '''Retruns info about configuration file for adduser and siblings (a la
    Fedora: "site-specific configuration for the shadow password suite."
    Returns None if configuration file is not found.'''
    conf_filename = None
    conf_opt_first_uid = None
    conf_opt_last_uid = None
    conf_opt_sep = None

    # 1) file name, 2) start of UID range option, 3) end of UID range option,
    # 4) separator in the file between option and value
    user_confs = (
        # Debian and Ubuntu
        ["/etc/adduser.conf", "FIRST_UID", "LAST_UID", "="],

        # Fedora (None means any whitespace)
        ["/etc/login.defs", "UID_MIN", "UID_MAX", None ]
    )

    for conf in user_confs:
        if os.path.isfile(conf[0]):
            conf_filename = conf[0]
            conf_opt_first_uid = conf[1]
            conf_opt_last_uid = conf[2]
            conf_opt_sep = conf[3]
            break

    return (conf_filename, conf_opt_first_uid, conf_opt_last_uid, conf_opt_sep)

def get_all_usernames():
    '''Returns non-system usernames if can get the system limits for normal
    user's UID. If not then returns all of the usernames in the system. The
    usernames are returned in a sorted list.'''

    if get_all_usernames.cached_names:
        return get_all_usernames.cached_names

    # inclusive range of UIDs for normal (non-system) users
    # by default this is the maximum possible range, that includes all users
    first_uid = 0
    last_uid = sys.maxint

    found_first_uid = False
    found_last_uid = False

    (conf_filename, conf_opt_first_uid, conf_opt_last_uid, conf_opt_sep) = \
        get_user_conf()

    # if got valid configuration file, try to get the UID range
    if conf_filename:
        for l in open(conf_filename):

            l = l.strip()

            if l.startswith(conf_opt_first_uid):
                first_uid = int(l.split(conf_opt_sep)[1])
                found_first_uid = True

            if l.startswith(conf_opt_last_uid):
                last_uid = int(l.split(conf_opt_sep)[1])
                found_last_uid = True

            # both found
            if found_first_uid and found_last_uid:
                break

    usernames = []

    for i in pwd.getpwall():
        # if UID not within the range, then skip this user
        if not first_uid <= i.pw_uid <= last_uid:
            continue
        usernames.append(i.pw_name)

    usernames.sort()

    get_all_usernames.cached_names = usernames

    return usernames

# will be set the first time function is executed, then it will be returned
# with all subsequent calls to save time and be consistent
get_all_usernames.cached_names = None

def remove_dir_tree(dir):
    '''Removes the specified directory recursively.'''

    MSGR.say("Removing directory %s" % dir, MSGR.SayTypeTask)

    if os.path.isdir(dir):
        shutil.rmtree(dir)
        MSGR.log("Removed directory %s" % dir)

    else:
        MSGR.log("Directory %s does not exist, not removed" % dir)

def add_sbox_to_groups():
    '''If the list of current supplementary groups does not have sbox group
    then will add it to that list. This handles the case of running under sudo,
    since the list of root's supplementary groups is used under it. Also,
    this handles the case of fresh installation, when the user does not belong
    to the sbox group at all.

    sbox group membership is required to run any Scratchbox command. sg cannot
    be used since it does not return the exit status of the executed
    process. Another option is newgrp command.

    NOTE: This function can only be used when running as root (because of
    setgroups).
    FIXME: root still remains in the supplementary group of a user process, has
    not cause troubles so far.'''
    assert running_as_root(), "Only root can add sbox to groups!"

    current_groups = os.getgroups()
    sbox_gid = grp.getgrnam(SB_GROUP).gr_gid

    if sbox_gid not in current_groups:
        new_groups = current_groups + [sbox_gid]
        os.setgroups(new_groups)

def set_eguid(username, need_sbox_group = False):
    '''Changes the effective user and group IDs, should be used to temporarily
    drop root privileges. Also sets environment variable needed by some of
    the executed processes.
    username = User whose credentials (UID & GID) will be used.
    need_sbox_group = if True, then sbox will be added to supplementary groups,
                      so that scratchbox commands can be executed.'''
    assert running_as_root(), "IDs can only be set as root!"
    
    if need_sbox_group:
        add_sbox_to_groups()

    pwd_ent = pwd.getpwnam(username)

    os.setegid(pwd_ent.pw_gid)
    os.seteuid(pwd_ent.pw_uid)
    os.environ['HOME'] = pwd_ent.pw_dir
    os.environ['USER'] = pwd_ent.pw_name

def set_guid(username, need_sbox_group = False):
    '''Changes the effective, real and saved set-user-ID user and group IDs,
    should be used to permanently drop root privileges.  Also sets environment
    variable needed by some of the executed processes.
    username = User whose credentials (UID & GID) will be used.
    need_sbox_group = if True, then sbox will be added to supplementary groups,
                      so that scratchbox commands can be executed.'''
    assert running_as_root(), "IDs can only be set as root!"
    
    if need_sbox_group:
        add_sbox_to_groups()

    pwd_ent = pwd.getpwnam(username)

    os.setgid(pwd_ent.pw_gid)
    os.setuid(pwd_ent.pw_uid)
    os.environ['HOME'] = pwd_ent.pw_dir
    os.environ['USER'] = pwd_ent.pw_name

def make_unique_filename(name_prefix):
    '''Returns a unique (non-existent) file name
    name_prefix = path & beginning of the name'''
    result = name_prefix
    index = 1

    # loop until unique name found
    while True:

        # name used, try another name
        if os.path.exists(result):
            result = name_prefix + "." + str(index)
            index += 1

        # found unique name
        else:
            break

    return result

def set_selinux_permissive(permanent):
    '''Sets the SELinux to Permissive mode. It is assumed that this function
    will only be called if SELinux is in Enforcing mode
    permanent = Boolean, if True, SELinux will be set to Permissive mode
                permanently, i.e. it will remain Permissive between reboots'''
    MSGR.say("Setting SELinux to Permissive mode", MSGR.SayTypeTask)
    conf_fn = "/etc/selinux/config"
    conf_enforcing = "SELINUX=enforcing"
    conf_permissive = "SELINUX=permissive"

    # set it to permissive for this boot session
    exec_cmd_fatal("setenforce 0")

    if not permanent:
        return

    # set it to permissive permanently, i.e. to remain permissive after reboot

    has_enforcing = file_has_line(conf_fn, r"^\s*" + conf_enforcing)
    MSGR.log("Setting SELinux to Permissive mode permanently "
             "(has_enforcing=%s)" % has_enforcing)

    file_backup(conf_fn)

    if has_enforcing:
        file_replace_text(conf_fn, conf_enforcing, conf_permissive)
    else:
        file_add_lines(conf_fn, [conf_permissive], log_lines = True)

def get_kernel_param(var, convert_to = None):
    '''Reads a kernel variable/parameter, and if succeeds returns its
    value. Returns None on errors.
    var = kernel variable to read
    convert_to = type to convert var to before returning'''
    result_err = None

    MSGR.log("Trying to read %s kernel parameter" % (var))

    p = subprocess_Popen(["sysctl", "-n", var],
                         stdout = subprocess.PIPE,
                         stderr = subprocess.STDOUT)

    val_str = p.communicate()[0].strip() # can be error message
    MSGR.log("Got string value %s" % (repr(val_str),))

    # systctl returned error code, probably variable not available
    if p.wait():
        MSGR.log("systctl failed reading %s variable" % (var))
        return result_err
    
    if convert_to:
        try:
            val_int = convert_to(val_str)
        except ValueError:
            MSGR.log("Was not able to convert %s to %s" %
                     (repr(val_str), convert_to))
            return result_err
        return val_int

    # not converting
    return val_str

def set_kernel_param(var, val, forever):
    '''Sets kernel parameter variable/parameter to the specified value.

    Even if setting permanently, will first try to set for current boot session
    only, since the some parameters (e.g. VDSO) are known to crash some systems
    (e.g. Ubuntu Gutsy). If this is not done the system might become unbootable
    because of permanent settings.
    var = kernel variable to set
    val = set variable to this value
    forever = if True parameter will be set permanently. By default it is
              set only for the current boot session.'''
    if forever:
        msg = "Setting %s permanently" % (var,)
    else:
        msg = "Setting %s for this session" % (var,)

    MSGR.say(msg, MSGR.SayTypeTask)

    # set it for this session only (will persist until the next boot)
    MSGR.log("Attempting to set %s=%s for this session" % (var, val))
    exec_cmd_fatal("sysctl -w %s=%s" % (var, val))

    # now set it permanently
    if forever:
        MSGR.log("Permanently setting %s=%s" % (var, val))
        file_add_lines(SYS_INFO.sysctl_conf_fn, ["%s = %s" % (var, val)])
        exec_cmd_fatal("sysctl -p")

def setup_target_resolv_conf(username, target_name):
    '''Copies resolv.conf into the target'''
    src = "%s/etc/resolv.conf" % (SB_PATH,)
    dst = "%s/users/%s/targets/%s/etc" % (SB_PATH, username, target_name)
    file_copy(src, dst)

def setup_target_sources_list(username, target_name):
    '''If alternatives sources.list is specified replaces sources.list in both
    targets with alternative one.'''
    if not OPTIONS.alt_sources_list:
        return

    sources_list_fn = ("%s/users/%s/targets/%s/etc/apt/sources.list" %
                       (SB_PATH, username, target_name))

    MSGR.log("Replacing sources.list in %s target of user %s" %
             (target_name, username))

    file_copy(OPTIONS.alt_sources_list, sources_list_fn)

def setup_target_apt_conf(username, target_name, proxy):
    '''Creates apt.conf for the target. If proxy is specified will also add
    proxy configuration to apt.conf file.'''
    apt_conf_fn = ("%s/users/%s/targets/%s/etc/apt/apt.conf" %
                   (SB_PATH, username, target_name))

    apt_conf_lines = []

    if CREDS_MGR: # disable verification only with the "secure" servers
        apt_conf_lines.append('Acquire::https::Verify-Peer "false";')
    if proxy:
        apt_conf_lines.append('Acquire::http::Proxy "%s";' % proxy)

    MSGR.log("Creating apt configuration in %s target of user %s" %
             (target_name, username))

    file_add_lines(apt_conf_fn, apt_conf_lines, "//",
                   os.path.exists(apt_conf_fn), True, username)

def setup_target_apt_cache(username, target_name, _exec_cmd):
    '''Updates apt cache (runs apt-get update) of the target. If secure
    repository servers are in use, netrc in sb must be configured by now.'''
    MSGR.log("About to update apt cache in %s target of user %s" %
             (target_name, username))

    sb_login = "%s/login" % (SB_PATH,)
    _exec_cmd("%s apt-get -o Acquire::http::TimeOut=15 -o "
              "Acquire::http::Retries=2 update" % sb_login)

def setup_target(username, target_name, rootstrap_path, toolchain, devkits,
                 cputransp, proxy):
    '''Sets up a Scratchbox target.
    username = user for whom set up the target
    target_name = target name
    rootstrap_path = path of the saved rootstrap file
    toolchain = name of toolchain
    devkits = string of devkits (colon separated)
    cputrans = CPU transparency method'''
    MSGR.say("Setting up target %s for user %s" % (target_name, username),
             MSGR.SayTypeTask)
    MSGR.log("Target setup parameters: username=%s, target_name=%s, "
             "rootstrap_path=%s, toolchian=%s, devkits=%s, cputrans=%s, "
              "proxy=%s" %
             (username, target_name, rootstrap_path, toolchain, devkits,
              cputransp, proxy))

    # AdminInstallCommand: SDK installation failure for one user is not fatal
    if running_as_root():
        _exec_cmd = lambda cmd: exec_cmd_error(cmd, username)

    # UserInstallCommand: SDK installation failure is fatal
    else:
        _exec_cmd = lambda cmd: exec_cmd_fatal(cmd)

    sb_conf = "%s/tools/bin/sb-conf" % (SB_PATH,)

    # setup
    setup_cmd = "%s setup %s --force --compiler=%s" % (sb_conf, target_name,
                                                       toolchain)
    if devkits:
        setup_cmd += " --devkits=%s" % (devkits)
    if cputransp:
        setup_cmd += " --cputransp=%s" % (cputransp)

    _exec_cmd(setup_cmd)

    # reset
    _exec_cmd("%s reset -f %s" % (sb_conf, target_name))

    # select
    _exec_cmd("%s select %s" % (sb_conf, target_name))

    # rootstrap
    _exec_cmd("%s rootstrap %s %s" % (sb_conf, target_name, rootstrap_path))

    # install
    _exec_cmd("%s install %s --etc --devkits --fakeroot --debug-links" %
              (sb_conf, target_name))

    setup_target_resolv_conf(username, target_name)
    setup_target_sources_list(username, target_name)
    setup_target_apt_conf(username, target_name, proxy)
    setup_target_apt_cache(username, target_name, _exec_cmd)

def gzip_integrity_ok(filename):
    '''Returns True if integrity of a zip file is OK'''
    MSGR.log("Testing integrity of file " + filename)

    result = not exec_cmd("gunzip -qt " + filename)

    if result:
        MSGR.log("Integrity test succeeded")
    else:
        MSGR.log("Integrity test failed")

    return result

def save_tarball(url, filename, desc, post_seq = None):
    '''Sets up the SDK tarball packages. A tarball will be downloaded if it is
    not found on the disk or if it is on the disk but the integrity check
    fails. If the integrity check of the downloaded tarball fails then an
    exception is raised, hence the whole SDK installation fails.
    url = URL of the tarball
    filename = path to the tarball, where it should be saved
    desc = description of the tarball
    post_seq = sequence of data to post in an HTTP POST operation'''
    MSGR.say("Setting up %s %s" % (desc, filename), MSGR.SayTypeTask)

    if os.path.isfile(filename): # try to use cached file
        MSGR.log("Tarball exists, will use it if integrity is OK")
        if gzip_integrity_ok(filename):
            return
        else:
            MSGR.log("Tarball failed integrity test, will download it")
            os.remove(filename)

    MSGR.log("Downloading %s tarball" % (desc,))
    url_save(url, filename, post_seq = post_seq)

    if not gzip_integrity_ok(filename):
        raise FriendlyException(
            "%s tarball %s failed integrity test!" % (desc, filename))

def sb_adduser(username):
    '''Adds a user to the Scratchbox.'''
    MSGR.say("Adding user %s to %s" % (username, SB_NAME), MSGR.SayTypeTask)

    # will fail if Scratchbox user account already exists
    exec_cmd('%s/sbin/sbox_adduser "%s" yes' % (SB_PATH, username))

    # symbolic link in Scratchbox home: user -> username
    user_home = "%s/users/%s/home/user" % (SB_PATH, username)

    if not os.path.exists(user_home):
        os.symlink(username, user_home)

def sb_exec_run_me_first():
    '''Runs the /scratchbox/run_me_first.sh and answers questions asked
    by it.'''
    MSGR.say("Running %s first time script" % (SB_NAME,), MSGR.SayTypeTask)

    run_me_first_sh = os.path.join(SB_PATH, "run_me_first.sh")
    run_me_first_done = os.path.join(SB_PATH, ".run_me_first_done")

    if not os.access(run_me_first_sh, os.X_OK):
        raise FriendlyException(
            "%s first time script is not executable. Something went wrong "
            "with the install. Sorry." % (SB_PATH))

    # upgrade
    if SYS_INFO.has_scratchbox and os.path.exists(run_me_first_done):
        os.remove(run_me_first_done)

    p = subprocess_Popen(run_me_first_sh,
                         shell = True,
                         stdin = subprocess.PIPE,
                         stdout = MSGR.fo_log,
                         stderr = subprocess.STDOUT)

    # answers to:
    # 1) Do you want to use sudo mode?
    # 2) Give the name of the scratchbox group
    # 3) Would you like me to create the group 'sbox' for you?
    answers = "no\n%s\nyes\n" % (SB_GROUP,)
    MSGR.log("Communicating '%s' to %s" % (repr(answers), run_me_first_sh))
    p.communicate(answers)
    p.wait()

    if p.returncode:
        raise Exception("%s failed!" % (run_me_first_sh,))

def sb_run_prermdir_sanity_checks():
    '''Runs sanity checks to verify that scratchbox directory can be safely
    removed.'''

    MSGR.say("Running %s directory remove sanity checks" % (SB_PATH),
             MSGR.SayTypeTask)

    # there should not be any scratchbox mounts
    if not exec_cmd("mount | grep '%s'" % (SB_PATH,)):
        raise CmdFatalFailure("%s mounts still exist, cannot remove "
                              "'%s' directory!" % (SB_NAME, SB_PATH))

def sb_run_postinst_sanity_checks():
    '''Runs Scratchbox post install sanity checks.'''

    MSGR.say("Running post install %s sanity checks" % (SB_NAME),
             MSGR.SayTypeTask)

    cmds = ("%s/sbin/sbox_adduser" % (SB_PATH,),
            "%s/sbin/sbox_ctl" % (SB_PATH,),
            "%s/login" % (SB_PATH,),
            "%s/compilers/bin/gcc" % (SB_PATH,))

    for cmd in cmds:
        if not os.access(cmd, os.X_OK):
            raise CmdFatalFailure(
                "%s command '%s' is not executable. Something went "
                "wrong with the install." % (SB_NAME, cmd))

def tq_append_tarball_install_sb_tasks(tasks):
    '''Appends Scratchbox installation from tarballs tasks to the tasks queue
    argument.
    tasks = an instance of CmdTasksQ'''
    # download
    for sb_file in SB_TARBALL_FILES:
        url = urlparse.urljoin(SB_TARBALL_URL, sb_file)
        fn = os.path.join(SB_FILE_DIR, sb_file)
        tasks.append(save_tarball, url, fn, "%s package" % (SB_NAME,))

    # create extract directory if it does not exist
    extract_dir = os.path.dirname(SB_PATH)
    if not os.path.isdir(extract_dir):
        tasks.append_named(
            "Creating %s extract directory %s" % (SB_NAME, extract_dir),
            os.makedirs, extract_dir)

    # install
    for sb_file in SB_TARBALL_FILES:
        fn = os.path.join(SB_FILE_DIR, sb_file)
        tasks.append_named(
            "Extracting %s to %s" % (fn, extract_dir),
            exec_cmd_fatal, "tar zxf %s -C %s" % (fn, extract_dir))

    # /scratchbox/run_me_first.sh
    tasks.append(sb_exec_run_me_first)

def tq_append_install_sb_tasks(tasks, install_selections):
    '''Appends Scratchbox installation tasks to the tasks queue argument.
    tasks = an instance of CmdTasksQ'''

    # there are SB packages for this host, so use package manager
    if PKG_MGR.pkg_name_product:
        tasks.append(PKG_MGR.product_install)
        tasks.append_nop() # says twice: for repo file and pkg installation

    # tarball installation (currently on systems without apt)
    else:
        tq_append_tarball_install_sb_tasks(tasks)

    tasks.append(sb_run_postinst_sanity_checks)

    for username in sorted(install_selections.usernames):
        tasks.append(sb_adduser, username)

def tq_append_setup_sb_bashrc(tasks, username, install_selections):
    '''If needed environment variables are missing from the bashrc of
    Scratchbox, then appends a task to the tasks queue that will put them to
    bashrc. Needed variables are: proxy and DISPLAY.
    tasks = an instance of CmdTasksQ'''
    bashrc_fn = ("%s/users/%s/home/%s/.bashrc" % (SB_PATH, username, username))
    bashrc_lines = []
    bashrc_exists = os.path.isfile(bashrc_fn)

    # environment variables to check and set in bashrc: pairs of (name, value)
    env_vars = (("DISPLAY", install_selections.display),
                (install_selections.proxy_env_var, install_selections.proxy))

    for var in env_vars:
        var_name = var[0]
        var_value = var[1]

        if not var_value: # not set, can't put to bashrc
            continue

        export_str = "export %s=" % var_name

        if not (bashrc_exists and file_has_line(bashrc_fn, "^" + export_str)):
            bashrc_lines.append("%s%s" % (export_str, var_value))

    if bashrc_lines:
        tasks.append_named(
            "Adding environment variables to %s" % (bashrc_fn,),
            file_add_lines, bashrc_fn, bashrc_lines, "#",
            bashrc_exists, # will create if file does not exist
            True, username)

def tq_append_setup_sb_netrc(tasks, username):
    '''In case secure repository access is used: if netrc file does not exist
    in the Scratchbox home of the user, but exists in the real home of the
    user, then appends a task to the tasks queue that will copy netrc from the
    home to the Scratchbox home of the user.
    tasks = an instance of CmdTasksQ

    [1] netrc_get_filename not used since that returns netrc for the default
    user, which is BAD if SDK is installed for multiple users.

    FIXME: Since currently this function is only used for internal testing, it
    is not perfect. If someone decides to use this function for other than
    testing purposes, then these fixes should be done to this function:
    - if netrc does not exist in users home either, then the credentials should
      be asked from the user and written to netrc in sb home
    - ask the user if it is OK to copy netrc
    - if copying netrc from users home to sb, verify that netrc in users home
      has valid entries, only netrc of the default user is validated by this
      script
    - even better: parse sources.list of the target (in setup_target, after
      rootstrap installation) and append to netrc in sb the credentials for
      that host, if netrc in users home does not have the required credentials,
      they must be asked'''
    if not CREDS_MGR: # secure servers are not used: netrc not needed
        return

    netrc_fn_sb = ("%s/users/%s/home/%s/.netrc" %
                   (SB_PATH, username, username))
    home_dir = os.path.expanduser("~" + username)
    netrc_fn_host = os.path.join(home_dir, '.netrc') # see [1] 

    if not os.path.isfile(netrc_fn_sb) and os.path.isfile(netrc_fn_host):
        tasks.append_named(
            "Copying %s to %s" % (netrc_fn_host, netrc_fn_sb),
            file_copy, netrc_fn_host, netrc_fn_sb, username)

def tq_append_install_sdk_tasks(tasks, install_selections):
    '''Appends SDK installation tasks to the tasks queue argument.
    tasks = an instance of CmdTasksQ'''
    targets = (TARGET_ARMEL, TARGET_X86)

    # download rootstraps
    for target in targets:
        if target.rootstrap_url_is_magic:
            post_seq = [("agree", "I accept")]
        else:
            post_seq = None

        tasks.append(save_tarball, target.rootstrap_url, target.rootstrap_fn,
                     target.rootstrap_arch + " rootstrap", post_seq)

    # setup targets
    for username in sorted(install_selections.usernames):
        tq_append_setup_sb_bashrc(tasks, username, install_selections)
        tq_append_setup_sb_netrc(tasks, username)

        for target in targets:
            tasks.append(target.setup, username, install_selections)

def sb_get_version():
    '''Returns Scratchbox version as string.'''
    config = IMPORTER["sb.config"]
    version = config.version()

    return version

def sb_has_sessions(username = None):
    '''Returns True if Scratchbox sessions exist for the user running the
    script. False otherwise. If username is not specified, default one will be
    used.'''
    if not username:
        username = get_default_username()

    if running_as_root(): # only root can change ids
        child_preexec_fn = lambda: set_guid(username, True)
    else:
        child_preexec_fn = None
        assert username == get_default_username(), \
            ("Not running as root, username should be %s" %
             (get_default_username(),))

    sess_list = []
    sb_conf = "%s/tools/bin/sb-conf" % (SB_PATH,)
    if not os.path.exists(sb_conf): # scratchbox not installed or broken
        MSGR.warn("%s does not exist, how to check sessions?" % (sb_conf,))
        return sess_list

    cmd = "%s/tools/bin/sb-conf ls -S | grep '^/dev/pts'" % (SB_PATH,)
    sess_list = subprocess_Popen("exec " + cmd,
                                 stdout = subprocess.PIPE,
                                 stderr = MSGR.fo_log,
                                 preexec_fn = child_preexec_fn,
                                 shell = True
                                 ).communicate()[0].splitlines()

    return len(sess_list) > 1

def sb_get_usernames():
    '''Returns a list of Scratchbox user names. Basically returns only real
    user name entries from the /scratchbox/users directory. If there are no
    user names an empty list will be returned'''
    path = os.path.join(SB_PATH, "users")
    entries = os.listdir(path)

    if not entries:
        return entries

    # verify that the directory entries are valid usernames
    usernames = []

    for username in entries:
        try: # no need to verify that it is a dir: this check verifies it all
            pwd.getpwnam(username)
        except KeyError, e:
            MSGR.warn("%s entry %s is not a real user name" % (path, username))
        else:
            usernames.append(username)

    return usernames

def sb_get_sessions_usernames():
    '''Returns list of user names that have running Scratchbox sessions.'''
    usernames = sb_get_usernames()
    session_usernames = [name for name in usernames if sb_has_sessions(name)]

    return session_usernames

def sb_has_devkit(devkit):
    '''Returns True if the specified devkit is installed under scratchbox.
    False otherwise'''
    path = os.path.join(SB_PATH, "devkits", devkit)
    return os.path.isdir(path)

def sb_get_missing_devkits():
    '''Returns list of missing devkits. Empty list will be returned in case
    there are no missing devkits'''
    devkits_str = TARGET_X86.devkits + ":" + TARGET_ARMEL.devkits
    devkits = set(devkits_str.split(":")) # duplicates are removed

    missing_devkits = []

    for devkit in devkits:
        if not sb_has_devkit(devkit):
            missing_devkits.append(devkit)

    return missing_devkits

def sb_has_toolchain(toolchain):
    '''Returns True if the specified toolchain is installed under scratchbox.
    False otherwise'''
    path = os.path.join(SB_PATH, "compilers", toolchain)
    return os.path.isdir(path)

def sb_has_cputransp(cpu_transp):
    '''Returns True if the specified CPU transparency method is installed under
    scratchbox. False otherwise'''
    path = os.path.join(SB_PATH, "devkits/qemu/bin", cpu_transp)
    return os.access(path, os.X_OK)

def sb_target_exists(username, target):
    '''Returns True if specified target for specified username exists in
    scratchbox.'''
    return os.path.isdir("%s/users/%s/targets/%s" %
                         (SB_PATH, username, target))

def sb_prefix_exists(username, prefix):
    '''Returns True if either armel or x86 target with specified prefix exists
    in scratchbox. The SDK installer creates targets by taking prefix and
    appending architecture.'''

    return sb_target_exists(username, prefix + TARGET_X86.name_postfix) or \
        sb_target_exists(username, prefix + TARGET_ARMEL.name_postfix)

def sb_prefix_exists_any(usernames, prefix):
    '''Same as above function but takes a list of usernames instead. Returns
    True if prefix exists for any of the users'''
    for username in usernames:
        if sb_prefix_exists(username, prefix):
            return True
    return False

def sb_get_prefix_exists_usernames(usernames, prefix):
    '''Walks through the list of passed usernames to get a list of usernames
    for which the target prefix exists. Returns that list.'''
    usernames_out = \
        [username for username in usernames \
             if sb_prefix_exists(username, prefix)]

    return usernames_out

def sb_get_nonexistent_prefix(usernames):
    '''Returns target name prefix that does not exist for all of the users in
    the given list'''
    nonexistent_prefix = Target.name_prefix + time.strftime("_%Y%m%d")

    while True:
        if sb_prefix_exists_any(usernames, nonexistent_prefix):
            nonexistent_prefix += "_"
        else:
            break

    return nonexistent_prefix

def netrc_prompt_save_creds(uri, netrc_fn, username, password):
    '''If user agrees, saves credentials into netrc.

    If there are credentials for this server in netrc, it means they are
    wrong/outdated. That is because, if credentials in netrc are valid, this
    function is not be called at all.  Hence, if credentials are in netrc they
    will be replaced after a backup of netrc is made. If credentials for this
    server are not in netrc, they will be appended.

    uri = URI to the repository, FQDN will be saved into netrc as the
          machine token
    netrc_fn = filename with full path of the netrc file
    username = will be saved into netrc as the login token
    password = will be saved into netrc as the password token'''

    MSGR.log("Trying to save credentials into netrc")

    # get the network location (FQDN)
    netloc = urlparse.urlparse(uri)[1]
    if not netloc:
        MSGR.log("Could not get netloc from URL %s" % uri)
        return

    old_password = None # old/invalid password from netrc

    # read the netrc file
    if os.path.exists(netrc_fn):
        try:
            nrc = netrc.netrc(netrc_fn)

            auth = nrc.authenticators(netloc)
            if auth:
                old_password = auth[2]

        # netrc file has invalid content
        except netrc.NetrcParseError, e:
            MSGR.log("Failed to parse netrc file, won't attempt to store "
                     "credentials.")
            return

    print_wrapped(
        "To avoid re-entering credentials while installing the %s, the "
        "credentials have to be saved in cleartext into ~/.netrc file. "
        "%s" % (PRODUCT_NAME, sc.warn_cleartext_creds))

    if not is_yes("Save cleartext credentials into netrc file %s" % (netrc_fn),
                  False):
        print
        return

    print "Writing credentials into %s\n" % netrc_fn

    # credentials exist in netrc but they are wrong - replace
    if old_password:
        MSGR.log("Replacing existing credentials in netrc")
        file_backup(netrc_fn, get_default_username())
        file_replace_text(netrc_fn, old_password, password)

    # credentials are not in netrc - append
    else:
        MSGR.log("Appending new credentials to netrc")

        file_add_lines(netrc_fn, ["machine %s login %s password %s" %
                                  (netloc, username, password)])

    # since might contain credentials, only the user should be able to read it
    os.chmod(netrc_fn, 0600)

    # the netrc belongs to the default user, set the ownership
    ent = get_default_user_ent()
    os.chown(netrc_fn, ent.pw_uid, ent.pw_gid)

def netrc_get_creds(uri, netrc_fn):
    '''Searches for credentials for the URL in the netrc file. On success,
    returns tuple of login name and password. Returns None on error.
    uri = URI
    netrc_fn = filename with full path of the netrc file, this file should
               exist when calling this function'''

    assert os.path.isfile(netrc_fn), "netrc file should exist!"

    MSGR.log("Trying to get credentials from netrc")

    error_ret = None # returned on error

    # get the network location (FQDN)
    netloc = urlparse.urlparse(uri)[1]

    if not netloc:
        MSGR.log("Could not get netloc from URL %s" % uri)
        return error_ret

    try:
        nrc = netrc.netrc(netrc_fn)
    except netrc.NetrcParseError, e:
        print "\nFailed to parse netrc file %s:\n%s" % (netrc_fn, str(e))
        MSGR.log(str(e))
        return error_ret

    auth = nrc.authenticators(netloc)

    if not auth:
        return error_ret

    return (auth[0], auth[2])

def netrc_get_filename():
    '''Returns path to the netrc file of the default user. This means if
    running as root netrc file of root won't be used, but the netrc file of the
    user that run su or sudo will be used.'''
    if netrc_get_filename.cached_name:
        return netrc_get_filename.cached_name

    username = get_default_username()

    home_dir = os.path.expanduser("~" + username)
    netrc_fn = os.path.join(home_dir, '.netrc')

    MSGR.log("Got netrc filename for the default user: %s" % (netrc_fn))

    netrc_get_filename.cached_name = netrc_fn

    return netrc_fn

# will be set the first time function is executed, then it will be returned
# with all subsequent calls to save time and be consistent
netrc_get_filename.cached_name = None

class URLOpenError(Exception):
    '''Exception raised if opening URL fails'''

    def __init__(self, code):
        '''Constructor
        code = currentry HTTP status code'''
        self.code = code

    def __str__(self):
        '''String representation method.'''
        return \
            "Failed opening URL: HTTP error code %d: %s" % \
            (self.code, 
             BaseHTTPServer.BaseHTTPRequestHandler.responses[self.code][0])

class URLOpenProgress(object):
    '''Helper class to store data for the cURL PROGRESSFUNCTION'''

    def __init__(self):
        '''Constructor.'''
        self.perc_start = -1.0 # initial value
        self.last_perc_done = self.perc_start # last printed progress
        self.fo = MSGR.fo_log # where to write progress to

    def update(self, download_total, download_done, upload_total, upload_done):
        '''Prints progress only if there is a change by an increment of one
        from the previous time. This is because this function is called by cURL
        many times a second, sometimes with the same values, which would result
        in unnecessarily huge log files.

        PROGRESSFUNCTION(download total, downloaded, upload total, uploaded) ->
        status'''

        # happens when just starting or Ctrl-C is pressed
        if download_total == 0.0:
            return

        perc_done = (float(download_done) / float(download_total)) * 100
        
        show_progress = False

        # after redirection counters restart
        if download_done == 0.0 and self.last_perc_done == 100.0:
            self.last_perc_done = self.perc_start

        # first time
        if self.last_perc_done == self.perc_start:
            show_progress = True

        # last time
        elif perc_done == 100.0 and self.last_perc_done != 100.0:
            show_progress = True

        # increment from last time
        elif (perc_done - self.last_perc_done) >= 1.0:
            show_progress = True

        if show_progress:
            self.fo.write("\rDownloading %s/%s bytes  (%.0f%%)" %
                          (download_done, download_total, perc_done))
            self.fo.flush()
            self.last_perc_done = perc_done

        return 0

class CurlManager(object):
    '''Manages a cURL object.'''

    def __init__(self, url, dest, username = None, password = None,
                 post_seq = None):
        '''Constructor.
        url = the URL, local file download is supported with the file:// scheme
        dest = download destination to write to, see __set_dest for details
        username = Username to use.
        password = Password to use.
        post_seq = sequence of data to post in an HTTP POST operation
        If either username or password is set to None and URL is https,
        credentials manager will be used.'''
        self.progress = URLOpenProgress()
        self.url_protocol = urlparse.urlparse(url)[0]
        self.url_is_https = self.url_protocol == "https"

        # for https URLs if creds unspecified, get them from the manager
        if not (username and password) and self.url_is_https:
            if CREDS_MGR:
                creds = CREDS_MGR.get_creds(url)
                username = creds[0]
                password = creds[1]

        pycurl = IMPORTER["pycurl"]
        if not 'https' in pycurl.version_info()[8]:
            MSGR.warn("https protocol is not supported by installed curl!")

        curl = pycurl.Curl()
        curl.setopt(pycurl.URL, url)

        if username and password:
            curl.setopt(pycurl.USERPWD, "%s:%s" % (username, password))
        curl.setopt(pycurl.CONNECTTIMEOUT, 30)
        curl.setopt(pycurl.NOPROGRESS, 0)
        curl.setopt(pycurl.PROGRESSFUNCTION, self.progress.update)
        # curl.setopt(pycurl.NOSIGNAL, 1)

        if self.url_is_https: # for the "wannabe secure" https servers (nrm)
            curl.setopt(pycurl.SSL_VERIFYPEER, 0)
            curl.setopt(pycurl.FOLLOWLOCATION, 1)
            curl.setopt(pycurl.UNRESTRICTED_AUTH, 1)

        if post_seq:
            post_fields = urllib.urlencode(post_seq)
            curl.setopt(pycurl.POSTFIELDS, post_fields)
            curl.setopt(pycurl.POST, 1)

        self.curl = curl
        self.__set_dest(dest)

    def __get_dest_text(self):
        '''Returns the text of the write destination'''
        if isinstance(self.dest, str):
            txt = open(self.dest)
        else:
            txt = self.dest.getvalue() # StringIO

        return txt

    def __set_dest(self, dest):
        '''Sets the download destination to write to. Currently can be either
        StringIO buffer or a string filename. Must be called before perform.'''
        pycurl = IMPORTER["pycurl"]

        if isinstance(dest, str):
            fo = open(dest, "wb")
            self.curl.setopt(pycurl.WRITEDATA, fo)
        elif isinstance(dest, cStringIO.OutputType):
            self.curl.setopt(pycurl.WRITEFUNCTION, dest.write)
        else:
            raise FriendlyException("Unsupported write destination!")

        self.dest = dest

    def perform(self):
        '''Starts the download. On HTTP errors raises an exception with the
        error info.'''
        pycurl = IMPORTER["pycurl"]
        curl = self.curl

        try:
            curl.perform()
        except pycurl.error, e:
            # handle proxy errors by displaying some help to the user
            err_code = e[0]
            err_msg = e[1]
            proxy_errors = (pycurl.E_OPERATION_TIMEOUTED, pycurl.E_COULDNT_CONNECT)

            if err_code in proxy_errors:
                exc_msg = "cURL error %d: %s" % (err_code, err_msg)

                proxy_env_var = self.url_protocol + "_proxy"

                if os.environ.has_key(proxy_env_var): # proxy is set
                    msg = ("%s is set to %s. Is it correct?" %
                           (proxy_env_var, os.environ[proxy_env_var]))

                else: # proxy is not set
                    msg = ("Do you need to set %s?" % (proxy_env_var,))

                raise FriendlyException("%s\n%s" % (exc_msg, msg))

        http_code = curl.getinfo(pycurl.HTTP_CODE)

        # server returned error code, raise exception: according to RFC 2616,
        # "2xx" code indicates that the client's request was successfully
        # received, understood, and accepted.
        # 0 is returned when using file:// scheme (downloading local file)
        if http_code != 0 and not (200 <= http_code < 300):
            MSGR.log("Failed to open URL, contents: %s" %
                     (self.__get_dest_text(),))
            raise URLOpenError(http_code)

def url_open(url, username = None, password = None):
    '''Creates a StringIO (file-like object) for the specified URL and returns
    it. For description of the parameters see doc string of the CurlManager's
    constructor.'''
    MSGR.log("Opening URL %s" % (url,))

    str_buf = cStringIO.StringIO()

    curl_mgr = CurlManager(url, str_buf, username, password)
    curl_mgr.perform()

    str_buf.seek(0)
    return str_buf

def url_save(url, filename = None, username = None, password = None,
             post_seq = None):
    '''Saves URL page into local file. Returns the file name.
    url = URL to download
    filename = Name of the file where to store the contents of URL. This file
               should not exist. If set to None a unique temporary file name
               will be used.
    post_seq = sequence of data to post in an HTTP POST operation
    For description of the other parameters see doc string of the CurlManager's
    constructor.'''
    MSGR.log("Saving URL %s as %s" % (url, filename))

    if not filename:
        filename = tempfile.mktemp()

    if os.path.exists(filename):
        raise Exception("File %s already exists!" % (filename))

    curl_mgr = CurlManager(url, filename, username, password, post_seq)
    curl_mgr.perform()

    return filename

def file_replace_text(filename, old_text, new_text):
    '''Replaces old_text with new_text in the specified file.
    NOTE: don't log text parameters, they could be passwords'''
    assert os.path.isfile(filename), "File %s does not exist!" % (filename)

    MSGR.log("Replacing text in %s" % (filename,))
    
    # read the contents of the file (not the most efficient way)
    in_fo = open(filename)
    lines = in_fo.readlines()
    in_fo.close()

    # write to the file
    out_fo = open(filename, 'w')

    for line in lines:
        out_fo.write(line.replace(old_text, new_text))

    out_fo.close()

def file_copy(src, dst, username = None):
    '''Copies a file.
    username = copying files as root gives ownership to root, if this name is
               specified, ownership is given to this user instead'''
    MSGR.log("Copying %s to %s" % (src, dst))
    shutil.copy(src, dst)

    if username:
        pwd_ent = pwd.getpwnam(username)
        os.chown(dst, pwd_ent.pw_uid, pwd_ent.pw_gid)

def file_backup(filename, username = None):
    '''Makes a back copy of a file. File name must exists. Returns the
    name of the backup copy.
    username = If specified, set the ownership of the backup to this user.'''
    assert os.path.isfile(filename), "File must exist!"

    time_stamp = time.strftime("%Y%m%d_%H%M%S")
    backup_filename = \
        make_unique_filename("%s.backup_%s" % (filename, time_stamp))

    MSGR.log("Will back up %s as %s" % (filename, backup_filename))
    file_copy(filename, backup_filename, username)

    return backup_filename

def file_has_line(filename, line_pat):
    '''Returns true if a line is found in the file.
    filename = name of the file
    line_pat = regular expression pattern for the line'''
    lines = open(filename).readlines()

    log_txt = "File " + filename + " %s line " + line_pat

    for line in lines:
        m = re.match(line_pat, line)

        if m:
            MSGR.log(log_txt % "has")
            return True

    MSGR.log(log_txt % "does not have")
    return False

def file_add_lines(filename, lines, comment_char = '#', append = True,
                   log_lines = False, username = None):
    '''Adds list of lines into a file. Newlines are added to each added
    line.
    filename = name of the file
    lines = list of lines
    append = if True, lines will be appended, otherwise file will be
             truncated before adding lines
    comment_char = character designating one-line comments in the file
    log_lines = If true lines will be logged. By default lines are not
                logged, since they might conatain sensitive data e.g.
                credentials.
    username = If specified and file is truncated/created will set the
               ownership of the file to this user. This is used to avoid
               creating new files with root as owner. When appending to
               a file ownership is preserved, thus in that case this
               parameter is ignored.'''
    MSGR.log("Adding lines into file: filename=%s, comment_char=%s, "
             "append=%s, log_lines=%s, username=%s" %
             (filename, comment_char, append, log_lines, username))

    if log_lines:
        MSGR.log(seq_to_str(lines))

    if append:
        mode = 'a'
        header_line = "\n" + comment_char + \
            " The following lines were inserted by the %s" % MY_NAME
    else:
        mode = 'w'
        header_line = comment_char + \
            " This file was created by the %s and may be overwritten" % MY_NAME

    fo = open(filename, mode)

    try:
        fo.write(header_line + '\n')

        for line in lines:
            fo.write(line + '\n')

    finally:
        fo.close()

    if not append and username:
        pwd_ent = pwd.getpwnam(username)
        os.chown(filename, pwd_ent.pw_uid, pwd_ent.pw_gid)

class HTTPSCredentialsManager(object):
    '''Manages credentials for the HTTPS authentication. See doc sting of
    __read_creds for details.'''

    def __init__(self):
        '''Constructor.'''
        # dictionary with netloc indices and (username, password) items
        self.__creds = {}

    def get_netloc(self, uri):
        '''Returns network location of the URI. That is the same as 'machine'
        in netrc.'''
        return urlparse.urlparse(uri)[1]

    def get_creds(self, uri = None):
        '''Returns the credentials (username, password tuple) for the given
        URI. If they have not been saved previously, they will be read. If uri
        is None, the first credentials will be returned.'''
        if not uri:
            return self.__creds[self.__creds.iterkeys().next()]

        assert uri.startswith("https://"), \
            "Malformed URI %s, must begin with https!" % uri

        netloc = self.get_netloc(uri)

        if not self.__creds.has_key(netloc): # read if they are not found
            self.__read_creds(uri)

        return self.__creds[netloc]

    def __save_creds(self, netloc, username, password):
        '''Saves the credentials for the given network location.'''
        self.__creds[netloc] = (username, password)

    def __authorization_ok(self, uri, username, password, creds_from_netrc):
        '''Verifies that credentials are valid by trying to access the
        resource.'''
        def print_status(status):
            print status
            MSGR.cli_progress_stop()

        # URI must end with / to get the directory listing, otherwise it
        # results in "Failed opening URL: HTTP error code 301: Moved
        # Permanently"
        if not uri.endswith("/"):
            uri += "/"

        print
        MSGR.say("Verifying authorization:", MSGR.SayTypeTask)
        pycurl = IMPORTER["pycurl"]

        auth_ok = False

        try:
            fo = url_open(uri, username, password)

        except pycurl.error, e: # cURL failure
            print_status("cURL ERROR")
            err_code = e[0]
            err_msg = e[1]

            exc_msg = "cURL error %d: %s" % (err_code, err_msg)
            raise FriendlyException(exc_msg)

        except URLOpenError, e: # application layer failure
            # handle HTTP authorization error, to re-ask the credentials
            if e.code == 401:
                print_status("FAILED")

                if creds_from_netrc:
                    print "Credentials in netrc are wrong."

                else: # user typed in the creds
                    print "You probably made a mistake, please try again."

            # other HTTP errors than 401
            else:
                print_status("HTTP ERROR")
                print "Connection attempt to '%s' failed:\n%s" % (uri, str(e))
                if not is_yes("Retry?"):
                    raise FriendlyException("Could not verify authorization...")

        else: # no exceptions - got valid credentials
            auth_ok = True
            print_status("OK")

        return auth_ok

    def __read_creds(self, uri):
        '''Reads the user credentials (username & password) for the specified
        HTTPS URI.

        First tries to get the credentials from the netrc file. If these don't
        exist or don't work then the user is asked to enter the credentials.

        Whenever some credentials are obtained, establishes connection with the
        repository server in order to verify that the user has entered valid
        credentials (the user could have mistyped). This is done because access
        to the repository is a prerequisite to the installation phase.

        uri = Used to verify the provided credentials, and to check netrc file
              for possible credentials'''
        netloc = self.get_netloc(uri)
        netrc_fn = netrc_get_filename()

        # message shown only once
        msg_info = get_wrapped(
            "This script needs to access %s repository (%s) in order to "
            "download rootstaps and packages. The repository requires client "
            "authentication by username/password to access its content. If found, "
            "the credentials from the netrc file (%s) will be used. Otherwise, "
            "you must provide these credentials for the installation to work." %
            (PRODUCT_NAME, netloc, netrc_fn))
        print
        print msg_info

        # try to read credentials from netrc
        if os.path.isfile(netrc_fn):
            creds = netrc_get_creds(uri, netrc_fn)

            if creds: # got valid result
                print "\nUsing credentials from %s" % (netrc_fn)
                username = creds[0]
                password = creds[1]

                if self.__authorization_ok(uri, username, password, True):
                    self.__save_creds(netloc, username, password)
                    return

        # netrc did not have valid credentials, read credentials from the user
        while True:
            print
            print_wrapped("\nPlease enter %s credentials." % netloc)
            username = raw_input("Username: ")
            password = getpass.getpass()

            if self.__authorization_ok(uri, username, password, False):
                netrc_prompt_save_creds(uri, netrc_fn, username, password)
                self.__save_creds(netloc, username, password)
                break

def credentials_manager():
    '''Credentials manager factory function'''
    creds_mgr = None

    uris = []

    # only root can install the Scratchbox
    if running_as_root():
        if PKG_MGR.repo_requires_authentication():
            uris.append(PKG_MGR.get_repo_uri())

        # no package manager - the SDK will be installed from tarballs
        elif isinstance(PKG_MGR, UnknownPkgManager):
            if SB_TARBALL_URL.startswith("https://"):
                uris.append(SB_TARBALL_URL)

    # rootstap base URL
    if Target.rootstrap_base_url.startswith("https://"):
        uris.append(Target.rootstrap_base_url)

    if uris:
        creds_mgr = HTTPSCredentialsManager()

        MSGR.log("Creating credsentials managers, these URIs need "
                 "authentication: %s" % (uris))

        for uri in uris:
            creds = creds_mgr.get_creds(uri)

    return creds_mgr

def find_version_str(fo):
    '''Searches for version string in file-like object (or any iterable
    object). If found returns the version string, otherwise raises an
    exception'''

    p = re.compile('MY_VERSION = "(.*)"')

    for line in fo:
        m = p.match(line)

        if m:
            return m.group(1)

    raise FriendlyException("Version string not found!")

def version_str_to_list(version_str, pat_type = ""):
    '''Converts version string to a list of version integers and returns that
    list. On failure raises an exception.
    version_str = version string
    pat_type = pattern type. SB_NAME for scratchbox version pattern, otherwise
               pattern for this scripts version will be used'''

    # use scratchbox version pattern
    if pat_type == SB_NAME:
        pat = r'(\d+)\.(\d+)\.(\d+)$'
    # use this script's version pattern
    else:
        pat = r'(\d+)\.(\d+)\.(\d+) HARMATTAN \(\$Revision: (\d+) \$\)$'
    m = re.match(pat, version_str)

    if m:
        return [int(i) for i in m.groups()]

    raise FriendlyException("Failed converting version string to list, "
                            "string did not match the pattern!")

def check_updates():
    '''Checks if the server has a newer version of the script. If yes, offers
    the user to download and run that update instead.'''

    MSGR.log("Checking for updates in %s" % MY_URL)
    
    # get versions
    try:
        server_version_str = find_version_str(url_open(MY_URL))

        server_version_list = version_str_to_list(server_version_str)
        my_version_list = version_str_to_list(MY_VERSION)

    except Exception, e: # (pycurl.error, FriendlyException), e:
        MSGR.log("Exception while checking for updates")
        MSGR.log(str(e))
        MSGR.log_exc()
        return

    MSGR.log("Version strings: Mine %s, Server %s" % (MY_VERSION,
                                                      server_version_str))

    MSGR.log("Version lists: Mine %s, Server %s" % (my_version_list,
                                                    server_version_list))

    # update is available
    if server_version_list > my_version_list:
        MSGR.say("Newer version of %s is available at:\n%s" % (MY_NAME, MY_URL))

        if is_yes("Download and run the new version?"):
            
            # last part of URL
            update_filename = "./" + MY_URL[MY_URL.rfind("/") + 1:]
            MSGR.log("Saving update into %s" % update_filename)

            if os.path.exists(update_filename):
                file_backup(update_filename, get_default_username())

            url_save(MY_URL, filename = update_filename)

            # make sure executable, or exec will fail
            os.chmod(update_filename, 0700)

            # if file does not already exist will belong to root, so fix that
            ent = get_default_user_ent()
            os.chown(update_filename, ent.pw_uid, ent.pw_gid)

            print "\n\n\nStarting the updated %s..." % (MY_NAME)
            os.execvp(update_filename, [update_filename] + sys.argv[1:])

    else:
        MSGR.log("Updates are not available")

def sigint_handler(signum, frame):
    '''Handler for SIGINT. Terminates the script and its children'''
    assert signum == signal.SIGINT, "Wrong signal!"

    MSGR.log("Keyboard interrupt received, will quit!")
    MSGR.say("Bye!")

    # TODO: can we invoke cancel dialog in the GUI????
    kill_all()

def become_process_group_leader():
    '''Makes sure the setup is a process group leader or its process group
    leader can be safely killed (e.g. kdesudo, sudo, "su -c"). Done so that
    setup could safely kill self, children, grandchildren etc, if needed.

    In most cases explicitly becoming group leader is not needed because the
    shells make a launched process a group leader.

    There are exceptions, like root :) clicking setup icon on her desktop, in
    which case the script will belong to the group of desktop, and aborting
    will kill the desktop.

    Running under kdesudo, sudo or "su -c" does not make the script a group
    leader, but in those cases the kdesudo, sudo or "su -c" are group leaders,
    so no need to make this script the leader. Also, explicitly making the
    script a group leader under sudo and "su -c" puts it to the background
    process group, which means it cannot read/write from the controlling
    terminal (gets SIGTTIN).

    Running under pexpect makes a script a session leader.

    [1] Using ps to obtain current process group leader command with all its
        arguments as a string, ps removes all extra spaces, so no need to
        grep. the process group ID (a.k.a. ID of the process group leader) is
        used instead of parent process ID because "kdesudo -c" and some others
        launch a sh sub-shell which belongs to their group. So in that case the
        setup is a grandchild of kdesudo.'''

    my_pid = os.getpid()
    my_pgid = os.getpgid(my_pid)
    my_ppid = os.getppid()

    MSGR.log("pid=%d, ppid=%d, pgid=%d" %  (my_pid, my_ppid, my_pgid))

    # already a process group leader
    if my_pid == my_pgid:
        MSGR.log("Cool! I'm a process group leader!")
    
    # not a process group leader
    else:
        MSGR.log("Too bad! I'm not a process group leader, will check if "
                 "need to become one!")

        # see [1]
        cmd = subprocess_Popen(["ps", "-p", str(my_pgid), "-o", "args="],
                               stdout = subprocess.PIPE,
                               stderr = subprocess.STDOUT
                               ).communicate()[0].strip()

        MSGR.log("Process group leader command %s" % (repr(cmd)))

        # process group leaders that can be safely killed
        safe_cmds = ["su -c", "kdesudo", "sudo"]

        for safe_cmd in safe_cmds:
            if cmd.startswith(safe_cmd):
                MSGR.log("Process group leader command is safe, starts with %s"
                         % (repr(safe_cmd)))
                break
        else:
            MSGR.log("Process group leader command is not safe, attempting "
                     "to become process group leader")
            os.setpgrp()
            MSGR.log("My new process group is %d" % (os.getpgid(my_pid),))

def fix_exec_path():
    '''Adds missing needed paths to the search path of exec calls. This became
    needed on systems where sbin was not in the path by default (suse, so
    sysctl could not be located)'''
    needed_paths = ["/sbin"]

    MSGR.log("Default path is %s" % os.environ["PATH"])
    MSGR.log("Default defpath is %s" %  os.defpath)

    env_path_list = os.environ["PATH"].split(os.pathsep)
    def_path_list = os.defpath.split(os.pathsep)
    tot_path_list = env_path_list + def_path_list
    added = False

    for path in needed_paths:
        if not os.path.exists(path):
            MSGR.log("Path %s does not exist, skipping" % path)
            continue

        if path not in tot_path_list:
            MSGR.log("Adding %s to path" % path)
            os.environ["PATH"] += os.pathsep + path
            added = True

    if added:
        MSGR.log("New path is %s" % os.environ["PATH"])

def initialize_testing():
    global MSGR
    global IMPORTER
    global PKG_MGR
    global SYS_INFO
    global TESTING_UI

    MSGR = Messenger()
    SYS_INFO = SysInfo()
    PKG_MGR = pkg_manager()
    IMPORTER = DynamicImporter()

def initialize():
    '''Initializes data used by the script. Raises exception if something
    fails.'''

    global MSGR
    global IMPORTER
    global PKG_MGR
    global SYS_INFO
    global TESTING_UI
    global CREDS_MGR

    MSGR = Messenger()

    parse_options()

    if os.environ.has_key(TESTING_UI_ENV_VAR):
        MSGR.log("Starting in UI testing mode")
        TESTING_UI = True

    signal.signal(signal.SIGINT, sigint_handler)

    become_process_group_leader()

    fix_exec_path()

    SYS_INFO = SysInfo()
    PKG_MGR = pkg_manager()

    IMPORTER = DynamicImporter()

    # TODO: add updates check support
    if False:
        if OPTIONS.check_updates:
            check_updates()

    cmd = create_command(ARGS)

    if cmd.is_install: # these are needed only during installation
        CREDS_MGR = credentials_manager()

    return (cmd)

def get_cmd_help_text(cmd_class):
    '''Returns help text for a given Command class.'''
    txt = "%s\t%s (aliases: %s)" % (cmd_class.name, cmd_class.desc,
                                    seq_to_str(cmd_class.aliases, ", ", ""))
    return txt

def parse_options():
    '''Parses the command line options'''
    global OPTIONS
    global ARGS
    global OPT_PARSER

    usage = 'Usage: %prog [command] [options]' + \
'''

  %s.

  Sets up the %s.

  Maintainer: %s %s
  Report bugs to: %s


Commands:
''' % (MY_NAME, PRODUCT_NAME, CONTACT_NAME, CONTACT_EMAIL, CONTACT_BUGS_URL)

    # add command descriptions to usage
    for cmd_class in create_command.sup_cmd_classes:
        usage += get_cmd_help_text(cmd_class) + "\n"


    OPT_PARSER = OptionParser(usage = usage, version = "%prog " + MY_VERSION)

    OPT_PARSER.add_option("-i",
                          "--interface",
                          type = "choice",
                          choices = [CHOICE_UI_QT, CHOICE_UI_CL],
                          default = CHOICE_UI_QT,
                          help = "User interface to use. '%s' for Qt based "
                          "GUI, '%s' for command line. " %
                          (CHOICE_UI_QT, CHOICE_UI_CL) +
                          "[default: '%default']")

    OPT_PARSER.add_option("-s",
                          "--sources-list",
                          default = "",
                          metavar = "PATH",
                          dest = "alt_sources_list",
                          help = "Alternative sources.list file for both "
                          "targets.")

# Updates are disabled for now
#     OPT_PARSER.add_option("-u",
#                           "--dont-check-updates",
#                           action = "store_false",
#                           dest = "check_updates",
#                           default = True,
#                           help = "Disables checking for updates")

    (OPTIONS, ARGS) = OPT_PARSER.parse_args()

    # check that alternative sources list exists
    if OPTIONS.alt_sources_list and \
            not os.path.isfile(OPTIONS.alt_sources_list):
        raise FriendlyException("Alternative sources.list %s does not exist!" %
                                (OPTIONS.alt_sources_list))

def main():
    '''Main.'''
    # try to create all objects
    try:
        cmd = initialize()

    except FriendlyException, e:
        print_wrapped(str(e))
        MSGR.log_exc()
        sys.exit(1)

    # create and run the UI
    ui = create_ui()

    if ui:
        ui.run_cmd(cmd)

    sys.exit(0)

if __name__ == "__main__":
    try:
        main()

    except Exception, e: # unhandled exception, last resort
        if MSGR:
            MSGR.log_exc()

        print "\n\n\n"
        print "Sorry... The %s just crashed!!!\n" % MY_NAME
        print_wrapped(sc.contact_info_on_problems())
        print
        
        # MSGR does not exist so traceback won't be logged
        if not MSGR:
            print "In either case remember to attach the following traceback:"
        else:
            print "Here is the crash traceback:"

        print
        print str(e)
        traceback.print_exc()

        sys.exit(3)

else:
    # TODO: log file will be in weird places
    # set argv before parsing options
    # sys.argv.append("userinstall")
    # print sys.argv
    # parse_options()
    initialize_testing()
