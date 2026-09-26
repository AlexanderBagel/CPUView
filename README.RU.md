[English](README.md) | **Русский**

Продвинутый CPU-View для Lazarus.
================

[![Boosty](https://img.shields.io/badge/Boosty-Support-orange?logo=boosty)](https://boosty.to/processmemorymap)

Внимание - версия BETA!!!

``` 
С 17 января 2025 года ежедневные обновления CPU-View приостановлены.

Весь основной функционал уже добавлен, осталось всего 4 шага,
каждый из которых потребует длительной разработки и не будет
выкладываться, чтобы не сломать текущее поведение CPU-View.

1. Редактор SIMD-регистров. (ГОТОВО)
2. Поддержка архитектуры ARM через GDB. (ГОТОВО)
3. Поддержка виджетов Carbon/Cocoa под macOS + LLDB. (Временно приостановлено)
```

### Установка и использование:
1. Скачайте FWHexView https://github.com/AlexanderBagel/FWHexView и соберите FWHexView.LCL.lpk
2. Откройте CPUView_win_x86_64_D.lpk (или CPUView_lin_x86_64_D.lpk для Linux на Intel, либо CPUView_lin_aarch64_D.lpk для Linux на ARM) и установите его в IDE (меню: Package->Install/Uninstall Packages)
3. Пересоберите IDE
4. В режиме отладки выберите меню "View->Debug Windows->CPU-View" или нажмите Ctrl+Shift+C
5. Пользуйтесь на здоровье

Если вы хотите вносить изменения в диалоги Cpu-View, вам также потребуется установить пакет FWHexView_D.LCL.lpk

### Лог отладки и Crash Dump:
Лог отладки хранится по следующему пути: «lazarus_path\config_lazarus\cpuview\debug.log».
Он создаётся при первом открытии диалога CPU-View и содержит все логи, добавленные в течение сессии (то есть до окончательного закрытия Lazarus).
Лог предыдущей сессии удаляется при запуске, поэтому в случае ошибки файл лога следует сохранить для дальнейшего анализа.
При возникновении исключения в текущий лог сохраняется CallStack.

Вы можете отключить логирование или сбор crash dump в настройках "Tools->Options->Environment->CPU-View".
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/settings.png)

### Пять активных редакторов:
1. Дизассемблер
2. Регистры
3. Дамп
4. Стек
5. Скрипт и подсказки
6. Утилиты

### Общие возможности:
* ОС: поддержка Windows и Linux через Gtk2 или Qt5
* Процессоры: Intel x86_64, ARM (AArch64)
* Полная поддержка контекста потока (базовые, x87 и SIMD регистры) в Windows и Linux
* Светлая и тёмная темы отображения
* Поддержка кросс-компиляции
* Поддержка переключения потоков с мгновенным изменением отображаемой информации об активном потоке
* Команда перехода к выбранному адресу в любом из окон
* Двунаправленный стек переходов в каждом редакторе
* Активные подсказки для каждого редактора

### Окно дизассемблера поддерживает:
* Вывод отладочной информации (включая PDB для Windows)
* Отображение направления переходов
* Подсветку активного перехода
* Подсветку выбранного регистра
* Отображение имён вызываемых функций вместо их адресов
* Смещения (включаются двойным кликом по колонке адреса)
* Подсказки по выбранной инструкции с меню перехода к каждому блоку полученной информации
* Подсветку инструкций для удобства чтения кода
* Точки останова (отображение и изменение)
* Отображение дизассемблера для каждого перехода во всплывающей подсказке

### Окно регистров:
* Содержит отладочную информацию по каждому регистру (RAX..R15)
* Отображение SIMD-регистров (XMM и YMM) в 12 режимах отображения
* Три режима отображения регистров x87 (ST-R-M)
* Побитовое представление регистров флагов EFLAGS, TagWord, StatusWord, ControlWord, MxCsr (включая декодированный TagWord на x64)
* Изменение значения ЛЮБОГО регистра и быстрое переключение флагов
* Два режима отображения (полный и компактный)
* Быстрая подсказка по активным инструкциям перехода
* Коды LastError и LastStatus с описанием (только для Windows)
* Подсветка изменённых регистров
* Подсветка и подсказки для проверенных адресов
* Подсказки по некоторым регистрам и флагам из внешней базы данных

### Стек поддерживает:
* Отладочную информацию
* Подсветку активного и предыдущих кадров
* Подсветку адреса возврата
* Смещения (двойной клик по колонке адреса)
* Выделение дубликатов
* Подсветку и подсказки для проверенных адресов

### Дамп поддерживает:
* Несколько окон дампа
* 17 режимов отображения (включая Long Double 80 бит)
* 6 режимов текстовой кодировки
* 5 режимов копирования (включая массив pascal)
* Подсветку и подсказки для проверенных адресов
* Быстрые переходы к найденным проверенным адресам (через Ctrl+Click)
* Смещения (двойной клик по колонке адреса)
* Выделение дубликатов
* Распознавание и подсветку адресов

### Утилиты

1. TraceLog - отображает все инструкции, на которых происходила остановка во время отладки в CpuView.
2. Exports - отображает список экспортируемых функций по библиотекам, загруженным в адресное пространство отлаживаемого процесса.
3. Memory Map - отображает карту памяти отлаживаемого процесса.
4. PDB Manager - отображает доступные отладочные PDB-файлы и позволяет загружать их с внешних серверов символов.

### Команды:

"?" - вычисляет результат выражения
Например (Intel): "? [RIP+EAX*2+123]" вывод [RIP+EAX*2+123] = [100003982] -> 35DC5E8D98948C3
Например (AArch64): "? [X0, #16]" вывод [X0, #16] = [0x7FF63580D0 (RW.)] -> 0x7FF7FB4450 (RW.)

"gmh", "getmodulehandle" - возвращает ImageBase библиотеки в отлаживаемом процессе
Например: "gmh user32" вывод "user32.dll" instance 7FFEFDDF0000. Path: C:\WINDOWS\System32\user32.dll

"gpa", "getprocaddress" - возвращает адрес процедуры в отлаживаемом процессе
например: "gpa user32:MessageBoxA" вывод "user32:MessageBoxA" address: 7FFEFDE7C5B0

"bp" - устанавливает новую точку останова (BreakPoint)
например: "bp user32:MessageBoxA" вывод "user32:MessageBoxA" address: 7FFEFDE7C5B0 breakpoint set

"bc" - удаляет ранее установленную точку останова (BreakPoint)
например: "bc user32:MessageBoxA" вывод "user32:MessageBoxA" address: 7FFEFDE7C5B0 breakpoint remove

Для команд gpa/getprocaddress/bp/bc имя библиотеки указывать не обязательно.
например: "gpa PeekMessageA" вывод "user32.dll:PeekMessageA" address: 7FFE289E3FC0

Допускается использование смещений после имени функции:
Например: "gpa Beep" вывод "KERNELBASE.dll:Beep" address: 7FFE26702B10
Теперь укажем смещение при установке брекпойнта - "bp Beep+12" вывод "KERNELBASE.dll:Beep+12" address: 7FFE26702B1C breakpoint set

### Внешний вид:

Светлая тема:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/light.png)

Тёмная тема:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/dark.png)

Синтаксис At&T:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/att.png)

AArch64:
![](https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/aarch64.png)

Активный переход, брекпойнты, умные подсказки для выбранных инструкций и их меню, подсветка регистров.

RegView:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/regview.png"/>

RegEditors:
<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/regeditors.png"/>

Stack:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/stackview.png"/>

Dump:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/dumpview.png"/>

Hints:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/hints.png"/>

https://github.com/user-attachments/assets/65bea692-c68c-4264-b4c6-74bf9d3f8c99

Информация PDB (только для Windows!):

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/pdb_asm.png"/>

Менеджер загрузки отладочных PDB-символов:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/pdb_manager.png"/>

Поддержку отладочных PDB-символов можно включить на соответствующей вкладке настроек:

<img src="https://raw.githubusercontent.com/AlexanderBagel/CPUView/main/img/pdb_settings.png"/>

### Поддержать проект

Если проект оказался полезен, вы можете поддержать его развитие на странице основного проекта автора:

[![Boosty](https://img.shields.io/badge/Boosty-Support-orange?logo=boosty)](https://boosty.to/processmemorymap)

### История изменений:

#### 1.0 Бета (26-09-2026)
- К отладочной информации добавлена поддержка отладочных PDB символов и их актуализация через менеджер загрузок.
- Возвращена совместимость со Stable версией Lazarus. 
- Исправлена проблема с DPI у редактора SIMD регистров.