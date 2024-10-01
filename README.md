Advanced CPU-View for Lazarus.
================

Attention - BETA, version!!!

### Setup and use: 
1. Download FWHexView https://github.com/AlexanderBagel/FWHexView and compile FWHexView.LCL.lpk
2. Open CPUView_D.lpk and install it in the IDE (menu: Package->Install/Uninstall Packages) 
3. Rebuild IDE
4. In debug mode select menu "View->Debug Windows->CPU-View" or press Ctrl+Shift+C
5. Enjoy

### Four active editors:
1. Disassembler
2. Registers
3. Dump
4. Stack

Common features:
* OS: Windows and Linux support via Gtk2 (Qt not tested)
* Proc: Intel x86_64 (ARM not yet implemented)
* Light and dark display themes
* Crosscompiling support (tested on Windows)
* Supports thread switching with instantaneous change of displayed information about the active thread

The disassembler supports:
* Output debugging information
* Jump direction display
* Active jump highlighting
* Highlighting of the selected register (in the register window)
* Displays the names of called functions instead of their addresses
* Commands to switch to dump or stack mode
* Offsets
* Hinting on the selected instruction with a menu to jump to each block of the received information
* Instruction coloring for easy code reading

Register window:
* Contains debugging information for each register (RAX..R15)
* Bitwise representation of EFLAGS, TagWord, StatusWord, ControlWord, MxCsr flag registers
* Change register value and fast flag switching
* Two display modes (full and compact)
* Three display modes for x87 registers (ST-R-M)
* Quick hint on active jump instructions
* Commands to jump to disassembler, dump, or stack mode

Stack supports:
* Debug information
* Active and previous frames highlighting
* Return address highlighting
* Offsets
* Commands to switch to disassembler or dump mode

Dump supports:
* Commands to switch to disassembler or stack mode
* Offsets
* Multiple dump windows (Not yet implemented)

### Appearance:

Light theme:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/light.png)

Dark theme:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/dark.png)

Active jump, breakpoints, smart hints for selected instructions and their menus:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/light2.png)

Register hightlight:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/highlight.png)

Full regview mode:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg1.png" height="500"/>

Short regview mode with FPU-STx regs (Rx and Mx available):

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg2.png" height="500"/>

Short regview mode with XMM regs (ymm and debug available):

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg3.png" height="500"/>

Various options for displaying registers:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg4.png" height="500"/>

Stack:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/stack.png" height="500"/>

Stack with offsets:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/stack2.png" height="500"/>
