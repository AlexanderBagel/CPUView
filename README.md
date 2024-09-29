Advanced ASM debugger project.
================

Attention - BETA, version!!!
Need: FWHexView https://github.com/AlexanderBagel/FWHexView

### Four active editors:
1. Disassembler
2. Registers
3. Dump
4. Stack

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
* Light and dark display themes

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
![1](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/light.png)

Dark theme:
![2](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/dark.png)

Active jump, breakpoints, smart hints for selected instructions and their menus:
![3](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/light2.png)

Register hightlight:
![4](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/highlight.png)

Full regview mode:
![5](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg1.png)

Short regview mode with FPU-STx regs (Rx and Mx available):
![6](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg2.png)

Short regview mode with XMM regs (ymm and debug available):
![7](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg3.png)

Various options for displaying registers:
![8](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/reg4.png)

Stack:
![9](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/stack.png)

Stack with offsets:
![10](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/stack2.png)
