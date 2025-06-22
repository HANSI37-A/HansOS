; ==================================================================
; MikeOS hansOS Clone Kernel
; This kernel replicates the behavior of the provided "hansOS" code.
; It displays a welcome message, provides a command prompt, and
; implements 'info', 'help', and 'clear' commands, along with
; displaying system hardware information.
; It is designed to be loaded by the MikeOS bootloader at 0x1000.
; ==================================================================

    BITS 16             ; We are in 16-bit real mode
    ORG 0000h         ; MikeOS typically loads kernels at this address

start:
    ; Set up segment registers.
    ; CS (Code Segment) is already pointing to our code.
    ; DS (Data Segment) and ES (Extra Segment) need to be set to CS
    ; so we can access our data correctly.
    mov ax, cs
    mov ds, ax
    mov es, ax
    ; Set up stack pointer (crucial for push/pop and subroutines)
    ; A common safe stack address in real mode, assuming nothing else uses it.
    mov sp, 0000h

    ; Initial screen clear
    call clear_screen

    ; Print the welcome message
    mov si, welcome_string
    call print_string

main_loop:
    call print_prompt

    ; Read user input into the user_input buffer
    mov di, user_input
    call read_string
    call new_line ; Add a newline after user types Enter

    ; --- Command Handling ---

    ; Compare user input with "info"
    mov si, user_input
    mov di, info_cmd
    call compare_strings
    cmp al, 1           ; compare_strings returns AL=1 for match
    je show_info        ; If match, jump to show_info

    ; Compare user input with "help"
    mov si, user_input
    mov di, help_cmd
    call compare_strings
    cmp al, 1
    je handle_help      ; If match, jump to handle_help

    ; Compare user input with "clear"
    mov si, user_input
    mov di, clear_cmd
    call compare_strings
    cmp al, 1
    je handle_clear     ; If match, jump to handle_clear

    ; If command not recognized
    mov si, unknown_cmd
    call print_string
    call new_line
    jmp main_loop

; ==================================================================
; KERNEL SUBROUTINES
; ==================================================================

; Subroutine: clear_screen
; Clears the screen and sets video mode to 80x25 text.
clear_screen:
    pusha               ; Save general-purpose registers
    mov ah, 0x00        ; Function: Set Video Mode
    mov al, 0x03        ; Mode: 80x25 text, 16 colors (clears screen)
    int 10h             ; Call BIOS video services
    popa                ; Restore general-purpose registers
    ret

; Subroutine: print_string
; Prints a null-terminated string to the screen.
; Input: SI points to the start of the string.
; Uses BIOS interrupt 10h, function 0Eh (Teletype Output).
print_string:
    pusha               ; Save general-purpose registers
    mov ah, 0Eh         ; Function: Teletype Output
.loop:
    lodsb               ; Load byte from [DS:SI] into AL, then increment SI
    cmp al, 0           ; Check if the character is the null terminator
    je .done            ; If it is, we're done
    int 10h             ; Print the character in AL
    jmp .loop           ; Loop for the next character
.done:
    popa                ; Restore general-purpose registers
    ret

; Subroutine: print_prompt
; Prints the "hansOS >> " prompt.
print_prompt:
    pusha
    mov si, prompt_string
    call print_string
    popa
    ret

; Subroutine: read_string
; Reads a line of input from the keyboard into a buffer.
; Input: DI points to the start of the buffer.
; Output: Buffer contains the null-terminated input string.
read_string:
    pusha               ; Save registers
    push bx             ; Save BX, used for backspace boundary
    mov bx, di          ; Store the original start of the buffer in BX for backspace check
.read_loop:
    mov ah, 00h         ; BIOS function: Read Character from Keyboard
    int 16h             ; Call BIOS keyboard services
    ; AL = ASCII code, AH = Scan code

    cmp al, 0Dh         ; Check for Enter key (Carriage Return)
    je .done_reading    ; If Enter, finish input

    cmp al, 08h         ; Check for Backspace key
    je .backspace       ; If Backspace, handle it

    ; Filter non-printable ASCII characters (e.g., control characters)
    ; Allow ASCII characters from space (32) to tilde (126)
    cmp al, 32
    jb .read_loop
    cmp al, 126
    ja .read_loop

    ; Store character and echo it to screen
    mov [di], al        ; Store character in buffer
    mov ah, 0Eh         ; Teletype Output (for echoing)
    int 10h             ; Print the character on the screen
    inc di              ; Increment DI to point to the next position in the buffer
    jmp .read_loop

.backspace:
    cmp di, bx          ; Check if DI is at the start of the buffer (can't delete beyond start)
    je .read_loop       ; If at start, do nothing on backspace
    dec di              ; Move DI back to the previous character
    mov byte [di], 0    ; Clear the character from the buffer (null it out)
    mov ah, 0Eh         ; Teletype Output
    mov al, 08h         ; ASCII for Backspace
    int 10h             ; Print backspace (moves cursor left)
    mov al, ' '         ; Print a space to erase the character visually
    int 10h
    mov al, 08h         ; Print backspace again to move cursor back after erasing
    int 10h
    jmp .read_loop

.done_reading:
    mov byte [di], 0    ; Null-terminate the string in the buffer
    pop bx              ; Restore BX
    popa                ; Restore the values of all general-purpose 16-bit registers from the stack.
    ret

; Subroutine: compare_strings
; Compares two null-terminated strings.
; Input: SI points to string 1, DI points to string 2.
; Output: AL = 1 if strings are equal, AL = 0 if not equal.
compare_strings:
    push si             ; Save SI
    push di             ; Save DI
.compare_loop:
    mov cl, [si]        ; Load character from SI (string 1) into CL
    mov ch, [di]        ; Load character from DI (string 2) into CH
    cmp cl, ch          ; Compare the characters
    jne .not_equal      ; If mismatch, fail

    cmp cl, 0           ; Check if the character is null (end of string)
    je .equal           ; If both reached null, success

    inc si              ; Move to next char in string 1
    inc di              ; Move to next char in string 2
    jmp .compare_loop

.equal:
    mov al, 1           ; Set AL to 1 for equal
    jmp .exit_compare

.not_equal:
    mov al, 0           ; Set AL to 0 for not equal

.exit_compare:
    pop di              ; Restore DI
    pop si              ; Restore SI
    ret

; Subroutine: new_line
; Prints a Line Feed (LF) then Carriage Return (CR) to the screen.
; (Matches hansOS's order)
new_line:
    pusha
    mov ah, 0x0E        ; Teletype Output
    mov al, 0Ah         ; ASCII for Line Feed (LF)
    int 10h
    mov al, 0Dh         ; ASCII for Carriage Return (CR)
    int 10h
    popa
    ret

; Subroutine: print_decimal
; Prints the decimal value in AX. (Assumes AX < 65536)
print_decimal:
    pusha               ; Save all general-purpose registers
    mov cx, 0           ; CX will count digits
    mov bx, 10          ; Divisor for decimal conversion

    ; Handle zero case explicitly, otherwise it won't print anything
    cmp ax, 0
    je .print_zero

    .divide_loop:
        mov dx, 0       ; Clear DX before division (AX:DX / BX)
        div bx          ; AX = AX / 10, DX = AX % 10
        push dx         ; Push remainder onto stack
        inc cx          ; Increment digit count
        cmp ax, 0       ; Check if quotient is zero
        jne .divide_loop ; Continue if not zero

    .print_loop:
        pop dx          ; Pop digit (remainder) from stack
        add dl, '0'     ; Convert digit to ASCII character
        mov ah, 0x0E    ; Teletype Output function
        mov al, dl      ; Character to print
        int 10h         ; Call BIOS to print
        dec cx          ; Decrement CX for the loop counter
        jne .print_loop ; Loop if CX is not zero

    jmp .done_decimal

.print_zero:
    mov al, '0'         ; Print '0' if input was zero
    mov ah, 0x0E
    int 10h

.done_decimal:
    popa                ; Restore all general-purpose registers
    ret

; Print ' KB' suffix
print_kb_suffix:
    pusha
    mov si, kb_label
    call print_string
    call new_line       ; hansOS adds newline after KB
    popa
    ret

; Print ' MB' suffix
print_M_suffix:
    pusha
    mov si, mb_label
    call print_string
    call new_line       ; hansOS adds newline after MB
    popa
    ret

; ==================================================================
; COMMAND HANDLERS
; ==================================================================

handle_clear:
    call clear_screen
    jmp main_loop

handle_help:
    call new_line
    mov si, help_str
    call print_string
    call new_line ; Add extra newline for consistency if help_str doesn't end with one
    jmp main_loop

show_info:
    call new_line ; Add a newline before showing info for better formatting
    call detect_cpu
    call detect_base_memory
    call detect_mid_memory
    call detect_high_memory
    call detect_hard_drives
    call detect_serial_ports
    call detect_serial_port1
    call detect_mouse
    call new_line ; Add a newline after all info is displayed
    jmp main_loop

; ==================================================================
; HARDWARE DETECTION SUBROUTINES (Based on hansOS logic)
; ==================================================================

detect_cpu:
    pusha
    ;------CPU vendor------
    mov si, vendor_label
    call print_string

    mov eax,0           ; Prepare CPUID to return vendor ID string
    cpuid               ; Call CPUID instruction

    ; After CPUID with EAX=0:
    ; EBX = first 4 letters of vendor string
    ; EDX = next 4 letters
    ; ECX = last 4 letters
    mov [cpu_vendor+0],ebx  ; Store from the first 4 bytes 0,1,2,3
    mov [cpu_vendor+4],edx  ; Store from the second 4 bytes 4,5,6,7
    mov [cpu_vendor+8],ecx  ; Store from the third 4 bytes 8,9,10,11
    mov byte [cpu_vendor+12], 0 ; Null-terminate the string
    mov si,cpu_vendor
    call print_string
    call new_line

    ;------CPU description (brand string)------
    mov si, cpu_desc_label
    call print_string

    ; Get first part of brand string
    mov eax, 0x80000002
    cpuid
    mov [cpu_type+0], eax
    mov [cpu_type+4], ebx
    mov [cpu_type+8], ecx
    mov [cpu_type+12], edx

    ; Get second part of brand string
    mov eax, 0x80000003
    cpuid
    mov [cpu_type+16], eax
    mov [cpu_type+20], ebx
    mov [cpu_type+24], ecx
    mov [cpu_type+28], edx

    ; Get third part of brand string
    mov eax, 0x80000004
    cpuid
    mov [cpu_type+32], eax
    mov [cpu_type+36], ebx
    mov [cpu_type+40], ecx
    mov [cpu_type+44], edx
    mov byte [cpu_type+48], 0 ; Null-terminate the string

    mov si, cpu_type
    call print_string
    call new_line
    popa
    ret

; Detect Base Memory (INT 12h)
detect_base_memory:
    pusha
    mov si, base_memory_label
    call print_string
    ; INT 12h returns base memory in AX (in KB)
    int 0x12
    mov [base_mem], ax  ; Store for total calculation later
    call print_decimal  ; Print AX (base memory)
    call print_kb_suffix ; Print ' KB' and newline
    popa
    ret

; Detect Extended Memory (INT 15h, AH=88h)
detect_mid_memory:
    pusha
    mov si, mid_memory_label
    call print_string
    mov ah, 0x88        ; BIOS function 0x88: Get Extended Memory Size
    int 0x15            ; AX = extended memory in KB (between 1MB and 16MB)
    mov [mid_mem], ax   ; Store for total calculation later
    call print_decimal  ; Print AX (extended memory)
    call print_kb_suffix ; Print ' KB' and newline
    popa
    ret

; Get Memory Above 16MB (INT 15h, AX=E801h) and total memory calculation
detect_high_memory:
    pusha
    mov si, high_memory_label
    call print_string

    ; Use INT 15h, AX=E801h for memory above 16MB
    ; Returns: AX = extended memory in KB below 1MB (not relevant here)
    ;          BX = extended memory in KB above 1MB (not relevant here)
    ;          CX = extended memory in 64KB blocks below 16MB (not relevant here)
    ;          DX = extended memory in 64KB blocks above 16MB
    ; CARRY FLAG set if not supported.
    mov ax, 0xE801
    int 15h
    jc .no_e801         ; If carry flag is set, function is unsupported

    cmp dx, 0           ; Check if DX (64KB blocks above 16MB) is zero
    je .no_e801         ; If zero, no high memory or unsupported effectively

    ; Calculate memory above 16MB in MB
    ; DX contains number of 64KB blocks above 16MB
    ; 64KB * DX blocks / 1024 KB/MB = (64/1024) * DX MB = (1/16) * DX MB
    mov ax, dx          ; Copy DX (64KB blocks) to AX for division
    mov cx, 16          ; Divide by 16 to get MB
    xor dx, dx          ; Clear DX for division
    div cx              ; AX = AX / 16 (Result is MB)
    mov [high_mem_mb], ax ; Store for total calculation later
    call print_decimal  ; Print the high memory in MB
    call print_M_suffix ; Print ' MB' and newline
    jmp .sum_total      ; Jump to calculate total memory

.no_e801:
    mov si, not_supported_str
    call print_string
    mov word [high_mem_mb], 0 ; Set high memory to 0 if not supported
    call new_line

.sum_total:
    mov si, total_memory
    call print_string

    ; Start with 0 for total memory in KB
    xor eax, eax

    ; Add Base Memory (in KB)
    movzx ebx, word [base_mem] ; Zero-extend word to EAX
    add eax, ebx

    ; Add Mid Memory (in KB)
    movzx ebx, word [mid_mem]
    add eax, ebx

    ; Add High Memory: Convert MB to KB and then add
    movzx ebx, word [high_mem_mb]
    imul ebx, 1024      ; Multiply MB by 1024 to get KB
    add eax, ebx        ; Add to total KB

    ; Convert total KB to MB for display
    mov edx, 0          ; Clear EDX for division
    mov ecx, 1024       ; Divisor for KB to MB conversion
    div ecx             ; EAX = EAX / 1024 (Result is MB in EAX)

    ; Print total memory in MB (AX contains the lower 16 bits of EAX)
    MOV EAX, 0
    call print_decimal
    call print_M_suffix ; Print ' MB' and newline
    popa
    ret

detect_hard_drives:
    pusha               ; Save all general purpose registers

    mov si, hdd_label   ; Show label: "Number of hard drives: "
    call print_string

    ; Set ES to BIOS Data Area segment (0x40)
    push es             ; Save ES
    mov ax, 0x0040
    mov es, ax

    ; Load number of hard disks from [ES:0x0075] (POST Byte for Hard Disk Drive count)
    mov al, [es:0x0075]
    mov ah, 0           ; Clear upper byte to make AX = AL

    pop es              ; Restore ES

    call print_decimal  ; Print number of hard drives in AX
    call new_line
    popa                ; Restore registers
    ret

; Detect serial ports from BIOS Data Area
detect_serial_ports:
    pusha
    push es             ; Save ES
    mov si, serial_port_label
    call print_string

    mov cx, 0           ; Counter for serial ports
    mov ax, 0x0040
    mov es, ax          ; ES points to BIOS Data Area
    mov di, 0x0000      ; DI is offset from ES, starting at COM1 address

.next_port:
    mov dx, [es:di]     ; Get the port address (word)
    cmp dx, 0           ; Check if port address is zero (means port not present)
    je .skip_port       ; If zero, skip this port count

    inc cx              ; Increment count if port address is non-zero
.skip_port:
    add di, 2           ; Move to next port address (each address is 2 bytes)
    cmp di, 8           ; Check if we've checked all 4 COM ports (0x0000, 0x0002, 0x0004, 0x0006)
    jl .next_port       ; Continue if there are more ports to check

    mov ax, cx          ; Move total count to AX for printing
    call print_decimal
    call new_line       ; Newline after printing count

    pop es              ; Restore ES
    popa
    ret

; Detect Serial Port 1 (COM1) Base I/O Address
detect_serial_port1:
    pusha
    mov si, serial_label
    call print_string

    push es             ; Save ES
    mov ax, 0x0040      ; BIOS Data Area segment
    mov es, ax

    mov ax, [es:0x0000] ; COM1 base address is at 0040:0000 (word)
    pop es              ; Restore ES

    call print_decimal  ; Print the COM1 base address
    call new_line
    popa
    ret

; Detect Mouse
detect_mouse:
    pusha
    mov si,mouse_label
    call print_string

    mov ax, 0x0000      ; Mouse reset & presence check function
    int 0x33            ; BIOS Mouse interrupt (AX will return status)

    cmp ax, 0           ; AX=0 indicates mouse not present
    je .no_mouse        ; If AX is 0, jump to not found

    ; Mouse is present (AX will be non-zero, e.g., number of buttons)
    mov si, mouse_found_str
    call print_string
    call new_line
    popa
    ret

.no_mouse:
    mov si, mouse_notfound_str
    call print_string
    call new_line
    popa
    ret


; ==================================================================
; KERNEL DATA
; ==================================================================

; --- Strings for display ---
welcome_string          db 'Welcome to hansOS by Hansi Tharaki!', 0
prompt_string           db 'hansOS >> ', 0
info_cmd                db 'info', 0
unknown_cmd             db "unknown Command", 0
help_cmd                db "help", 0
clear_cmd               db "clear", 0

vendor_label            db 'CPU Vendor: ', 0
cpu_desc_label          db "CPU description: ", 0
base_memory_label       db "Base Memory: ", 0
mid_memory_label        db 'Extended Memory between (1M - 16M): ', 0
high_memory_label       db "Extended memory above 16M : ", 0
total_memory            db "Total usable memory: ", 0
not_supported_str       db 'Not supported', 0
hdd_label               db "Number of hard drives: ", 0
serial_port_label       db "Number of serial port: ", 0
serial_label            db "COM1 Base I/O Address: ", 0
mouse_label             db "Mouse status: ", 0

; Note: hansOS help_str had 0Dh, 0Ah embedded. I'll maintain that.
help_str                db "info-Hardware information", 0Dh, 0Ah, "clear-Clear Screen", 0Dh, 0Ah, 0
mouse_found_str         db "Found", 0
mouse_notfound_str      db "Not Found", 0
kb_label:               db ' KB', 0
mb_label:               db " MB", 0

; --- Buffers and variables for data storage ---
user_input              times 32 db 0   ; Buffer for user input (max 31 chars + null)
cpu_vendor              times 13 db 0   ; Buffer for CPU vendor string (12 chars + null)
cpu_type                times 49 db 0   ; Buffer for CPU brand string (48 chars + null)

base_mem                dw 0            ; Store base memory in KB
mid_mem                 dw 0            ; Store extended memory 1-16MB in KB
high_mem_mb             dw 0            ; Store extended memory >16MB in MB (calculated from blocks)
; The hansOS code had high_mem and total_mem as dw 0, but they were not strictly used for storage
; I've removed `high_mem` and `total_mem` here as their effective usage was within calculations.
; `high_mem_64k_blocks` was also temporary.
