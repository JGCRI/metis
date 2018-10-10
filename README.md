

# Introduction

- This guide does not replace the more detailed documents and links provided below. It is meant to summarize some of the key issues for visitors when they come and first set up GCAM.
- For any updates or corrections to this guide please contact zarrar.khan@pnnl.gov
- For github documents go to https://github.com/zarrarkhan/00JGCRIGuides/tree/master/GCAMGuide  


# Helpful Links and Resources

- **GCAM:**
    + Installing GCAM on PIC (confluence access required): https://confluence.pnnl.gov/confluence/display/JGCRI/Setting+up+GCAM+on+PIC
    + GCAM User Documentation : http://jgcri.github.io/gcam-doc/
    + GCAM Documentation TOC: http://jgcri.github.io/gcam-doc/toc.html
    + Model Overview http://jgcri.github.io/gcam-doc/overview.html
    + GCAM User Guide: http://jgcri.github.io/gcam-doc/user-guide.html
    + Tutorials:
        - 2017 Tutorial Slides: http://www.globalchange.umd.edu/data/annual-meetings/2017/GCAM_Tutorial_2017.pdf
        - Lecture 3: Intro to GCAM: https://www.youtube.com/watch?v=xRF9lFwtMr0
        - Lecture 4: GCAm Tutorial: https://www.youtube.com/watch?v=S7vAShH-dbs
- **Tethys:** https://github.com/JGCRI/tethys
- **Xanthos:** https://github.com/JGCRI/xanthos
- **Demeter:** https://github.com/immm-sfa/demeter

# Key Software & Terms

- **PIC:** PIC (PNNL Institutional Computing) is the PNNL supercomputer: https://confluence.pnnl.gov/confluence/display/PICHELP/PIC+Help+Home
- **PuTTY:** PuTTY is the SSH client used to securely access PIC: 	https://www.chiark.greenend.org.uk/~sgtatham/putty/
- **WinSCP:** WinSCP is a file managing software to easily manage files between your PIC account and your local desktop:	https://winscp.net/eng/download.php


# Accounts and Access
- **PIC Issues:** Contact PNNL Computing Help: pic-support@pnnl.gov
- **Complete Training:** 
    - Need to complete training to receive username for PIC	
    - You will receive an email from administration with a link to http://elearner.pnnl.gov
    - In the email you will have a temporary UN and PW
- **Receive PIC Username:** 
    - A PNNL host will request a PIC account for you at https://iops.pnl.gov/iops/mainmenu.html?c_role=default
    - An email will be sent from a PNNL administrator to you confirming your new account and username
- **Create SecureID PIN:**
    - When you arrive you will have received a SecureID token generator. This is a physical device like a USB stick which displays a 6 digit token. This token continuously changes and will become part of your new PW. You will use the secure token to generate a new SecureID PIN
        - Go to https://portal.pnnl.gov
        - Enter your username and in the SecureID Field enter the SecureID token from your token generator. Leave the PW field blank.
        - You will be put into a PIN setting screen, where you enter your choice of PIN twice. PINs must be 6 digits
        - Once your PIN is set, the system will ask for PASSCODE. This PASSCODE will be your PIN followed by the new six digits from the token (which constantly change). For example, if the PIN is xxxxx and 6 digits from token are 123456, the PASSCODE is xxxxx123456.
- **Login to PIC:**
    - Now you can login to PIC by opening PuTTY and entering the correct connection settings with your UN and PW.	
    - Open PuTTY and do the following:
        - In "Host Name (or IP address)" enter "constance.pnl.gov"
        - In "Port" enter "22"
        - Leave everything else as is and click "Open"
        - A command prompt will open and ask for login. Enter your username fro PIC
        - Then enter your PASSCODE as created in the step above.
        
![PuTTY Configuration](/READMEfigs/puttyConfig.png)

- **Login to WinSCP:**
    - Now you can login to WinSCP and connect to your folder on PIC.
    - After you login you will be able to view folders on your desktop on the left and your folder on the PIC supercomputer on the right.
    - Open WinSCP and do the following:
        - In "Host name" enter "constance.pnl.gov"
        - In "Port number" enter "22"
        - In "User name" enter your username
        - In "Password" enter your PASSCODE
        
![PuTTY Configuration](/READMEfigs/WINSCPConfig.png)