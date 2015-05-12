; Snake multiplayer game
;	written by quckly.ru
;	v. 0.1a

	org	100h
	jmp	Main

; Video const
	VideoW = 320
	VideoH = 200
	VideoSegment = 0A000h
	;offset = (y<<8) + (y<<6) + x;
	
; Constants
	CellSize = 6
	FieldW = 53
	FieldH = 28
	FieldOffsetR = (VideoW - (FieldW * CellSize))/2
	FieldOffsetU = VideoH - (FieldH * CellSize)
	BodySize = 1500
	

; Structures
struc CSnake _x, _y, _b1, _b2, _b3
{
	.dirX	db _x
	.dirY	db _y
	.oldDX	db _x
	.oldDY	db _y
	.body	dw _b1, _b2, _b3
			dw BodySize dup(0)			; 0XXYYh
	.tail	dw 0
	.head	dw 2
	.length	dw 3
	
	.size	=	$ - .dirX
}

; bx + CSnake.field => Snake.field
virtual at bx
Snake	CSnake	?, ?, ?, ?, ?
end virtual

; Global vars
Old8:
	dw 0, 0

snake1	CSnake	0, 0FFh, 2311h, 2310h, 230Fh
snake2	CSnake	0, 001h, 0F0Fh, 0F10h, 0F11h

Player1Score	dw 0
Player2Score	dw 0

BonusXY			dw 0FFFFh	; X:Y
BonusID			dw 0

GameOver		dw 0
GameWinner		dw 0

GameStateExit = 0
GameStateMenu = 1
GameStateGame = 2
GameStateOver = 3
GameStatePause = 4
GameState		dw ?

; Random
R_seed	dw 0

; Constants
MenuHelloText	db 'Snake multiplayer game!', 10, 13, 'Press any key to start...$'
TextGameOver	db 'Game over!'
TextGameOver.sizeof = $ - TextGameOver

TextWinnerSnake	db ' snake - the winner!$'
TextRoundDraw	db 'Round draw ;($'

TextPlayer1		db 'Player 1: $'
TextPlayer2		db 'Player 2: $'

BMP_EmptyCell	db	00h, 00h, 00h, 00h, 00h, 00h
				db	00h, 00h, 00h, 00h, 00h, 00h
				db	00h, 00h, 00h, 00h, 00h, 00h
				db	00h, 00h, 00h, 00h, 00h, 00h
				db	00h, 00h, 00h, 00h, 00h, 00h
				db	00h, 00h, 00h, 00h, 00h, 00h

				; Snake 1
BMP_Snake1Head	db	001h, 0E0h, 0E0h, 0E0h, 0E0h, 001h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 020h, 020h, 0E0h, 0E0h
				db	0E0h, 0E0h, 020h, 020h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	001h, 0E0h, 0E0h, 0E0h, 0E0h, 001h
				

BMP_Snake1Tail	db	001h, 0E0h, 0E0h, 0E0h, 0E0h, 001h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0B0h, 0B0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0B0h, 0B0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	001h, 0E0h, 0E0h, 0E0h, 0E0h, 001h
				

BMP_Snake1Body	db	001h, 0E0h, 0E0h, 0E0h, 0E0h, 001h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 022h, 022h, 0E0h, 0E0h
				db	0E0h, 0E0h, 022h, 022h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	001h, 0E0h, 0E0h, 0E0h, 0E0h, 001h
					
				; Snake 2
BMP_Snake2Head	db	004h, 0E0h, 0E0h, 0E0h, 0E0h, 004h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 028h, 028h, 0E0h, 0E0h
				db	0E0h, 0E0h, 028h, 028h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	004h, 0E0h, 0E0h, 0E0h, 0E0h, 004h
				

BMP_Snake2Tail	db	004h, 0E0h, 0E0h, 0E0h, 0E0h, 004h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0B8h, 0B8h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0B8h, 0B8h, 0E0h, 0E0h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	004h, 0E0h, 0E0h, 0E0h, 0E0h, 004h
				

BMP_Snake2Body	db	004h, 0E0h, 0E0h, 0E0h, 0E0h, 004h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	0E0h, 0E0h, 02Ah, 02Ah, 0E0h, 0E0h
				db	0E0h, 0E0h, 02Ah, 02Ah, 0E0h, 0E0h
				db	0E0h, 0E0h, 0E0h, 0E0h, 0E0h, 0E0h
				db	004h, 0E0h, 0E0h, 0E0h, 0E0h, 004h
				
BMP_SnakeDead	db	04h, 00h, 00h, 00h, 00h, 04h
				db	00h, 04h, 00h, 00h, 04h, 00h
				db	00h, 00h, 04h, 04h, 00h, 00h
				db	00h, 00h, 04h, 04h, 00h, 00h
				db	00h, 04h, 00h, 00h, 04h, 00h
				db	04h, 00h, 00h, 00h, 00h, 04h

				; Apple
BMP_BonusApple	db	000h, 06Fh, 06Fh, 000h, 074h, 078h
				db	000h, 06Fh, 070h, 06Fh, 0BBh, 000h
				db	06Fh, 070h, 070h, 070h, 06Fh, 06Fh
				db	06Fh, 070h, 070h, 070h, 070h, 06Fh
				db	000h, 06Fh, 070h, 070h, 06Fh, 000h
				db	000h, 000h, 06Fh, 06Fh, 000h, 000h
				
				; Banan
BMP_BonusBanan	db	000h, 074h, 074h, 000h, 074h, 078h
				db	000h, 074h, 02Ch, 074h, 0BBh, 000h
				db	074h, 02Ch, 02Ch, 02Ch, 074h, 074h
				db	074h, 02Ch, 02Ch, 02Ch, 02Ch, 074h
				db	000h, 074h, 02Ch, 02Ch, 074h, 000h
				db	000h, 000h, 074h, 074h, 000h, 000h

; Macros
macro push [arg] { push arg }
macro pop [arg] { pop arg }

; Stack: A: CSnake
; Output: Stack: A - Y, X
GetNextPoint:
	push bp
	mov bp, sp
	push ax
	push bx
	push si
	
	mov bx, [bp+4]
	
	mov si, [Snake.head]
	shl si, 1					; *= sizeof(body[0])
	mov ax, [Snake.body + si]
	add ah, [Snake.dirX]
	add al, [Snake.dirY]
	
	; Check Y:
.CheckY:
	cmp al, 0FFh
	jz .SetYEnd
	
	cmp al, FieldH
	jnz .CheckX
	
	mov al, 0
	jmp .CheckX
	
.SetYEnd:
	mov al, FieldH - 1
	
.CheckX:
	cmp ah, 0FFh
	jz .SetXEnd
	
	cmp ah, FieldW
	jnz .Return
	
	mov ah, 0
	jmp .Return
	
.SetXEnd:
	mov ah, FieldW - 1
	
.Return:
	
	mov [bp+4], ax
	
	pop si
	pop bx
	pop ax
	leave
	ret

; Stack: A: CSnake
RenderSnake:
	push bp
	mov bp, sp
	pusha
	
	mov bx, [bp+4]	;	Get snake from stack, and set global Snake structure
	
	mov cx, [Snake.length]
	sub cx, 2
	mov si, [Snake.tail]
	shl si, 1	; *= dw
	
	; Render Tail
	mov ax, [Snake.body + si]
	
	cmp bx, snake2
	jz @f
	push BMP_Snake1Tail
	jmp .DrawTail
@@:
	push BMP_Snake2Tail
.DrawTail:
	call DrawCell
	
	add si, 2
	
.RS_For:
	mov ax, [Snake.body + si]
	
	cmp bx, snake2
	jz @f
	push BMP_Snake1Body
	jmp .DrawBody
@@:
	push BMP_Snake2Body
.DrawBody:
	call DrawCell
	
	add si, 2				; Add sizeof(body[0])
	
	loop .RS_For			; for --[Snake.length] != 0
	
	; Render Head
	mov ax, [Snake.body + si]
	
	cmp bx, snake2
	jz @f
	push BMP_Snake1Head
	jmp .DrawHead
@@:
	push BMP_Snake2Head
.DrawHead:
	call DrawCell
	
	popa
	leave
	ret	2
	
; Stack:	CSnake, const Point dw
; Return:	CSnake -> Bool, Point dw
CollidePointSnakeWithoutHead:
	push bp
	mov bp, sp
	pusha
	
	; mov word[bp+4], 1
	; jmp .Exit
	
	mov bx, [bp+4]
	
	mov cx, [Snake.length]
	dec cx	;	Collide without Head of snake
	mov si, [Snake.tail]
	;shl si, 1	; *= dw
	
	mov di, [bp+6]		; Point
@@:
	; mov ax, si
	; xor dx, dx
	; mov si, BodySize
	; div si
	; mov si, dx					; Index %= BodySize
	cmp si, BodySize
	jnz .AfterMod
	mov si, 0
.AfterMod:	
	shl si, 1	; *= dw

	mov ax, [Snake.body + si]
	cmp ax, di
	jz .Collide
	
	shr si, 1
	inc si
	
	loop @b
	
	jmp .NotCollide

.Collide:
	popa
	or ax, 1
	leave
	ret 4
	
.NotCollide:
	
.Exit:
	popa
	leave
	ret 4

; Input:
;	Stack: CSnake, NextPoint dw, EatBonus dw
;			bp+4		bp+6		bp+8
MoveSnake:
	push bp
	mov bp, sp
	pusha
	
	mov bx, [bp+4]
	
	mov ax, [bp+8]
	or ax, ax
	jnz .NotShiftTail
	
	; ReDraw tail:
	mov si, [Snake.tail]
	shl si, 1	; *= dw
	
	mov ax, [Snake.body + si]
	push BMP_EmptyCell
	call DrawCell				; Remove Tail on screen
	
	shr si, 1					; Return index
	inc si						; To next body cell
	
	mov ax, si
	mov dx, 0
	mov si, BodySize
	div si
	mov si, dx					; Index %= BodySize
	
	mov [Snake.tail], si
	shl si, 1
	
	; Draw next tail
	mov ax, [Snake.body + si]
	
	cmp bx, snake2
	jz @f
	push BMP_Snake1Tail
	jmp .DrawTail
@@:
	push BMP_Snake2Tail
.DrawTail:
	call DrawCell
	
.NotShiftTail:
	
	; ReDraw head:
	mov si, [Snake.head]
	shl si, 1		; *= dw
	
	mov ax, [Snake.body + si]
	cmp bx, snake2
	jz @f
	push BMP_Snake1Body
	jmp .DrawBody
@@:
	push BMP_Snake2Body
.DrawBody:
	call DrawCell
	
	shr si, 1					; Return index
	inc si						; To next body cell
	
	mov ax, si
	mov dx, 0
	mov si, BodySize
	div si
	mov si, dx					; Index %= BodySize
	
	mov [Snake.head], si
	shl si, 1
	
	; Draw next head
	mov ax, [bp+6]
	mov [Snake.body + si], ax	; Set head to next point
	
	cmp bx, snake2
	jz @f
	push BMP_Snake1Head
	jmp .DrawHead
@@:
	push BMP_Snake2Head
.DrawHead:
	call DrawCell
	
	; Save current directions
	xor ax, ax
	mov al, [Snake.dirX]
	mov [Snake.oldDX], al
	mov al, [Snake.dirY]
	mov [Snake.oldDY], al
	
	popa
	leave
	ret 6
	
TimerTickFlag	dw	0
TimerTick:
	pusha
	push bp
	mov bp, sp
	;
	
	mov ax, [TimerTickFlag]
	or ax, ax
	jz .FlagZero
	dec [TimerTickFlag]
	jmp .Exit
.FlagZero:
	mov [TimerTickFlag], 2
	
	; Game Logic Here
	
	; Check state
	cmp [GameState], GameStateGame
	jnz .Exit
	
	; Show "Game Over", and change state
.CheckGameOver:
	cmp [GameOver], 1 
	jnz .GameLoop
	; call SetTxtMode
	
	; mov ax, 0200h			; Set Cursor
	; xor bx, bx
	; mov dx, 00B0Fh
	; int 10h
	
	; mov dx, TextGameOver
	; mov ax, 0900h
	; int 21h
	
	mov ax, 1300h					; /* Print "Game Over"
	mov bx, 0Eh
	mov cx, TextGameOver.sizeof
	mov dx, 00B0Fh
	
	push es
	push bp
	push ds
	pop es
	mov bp, TextGameOver
	int 10h
	pop bp
	pop es							; */
	
	cmp [GameWinner], 0
	jz .PrintRoundDraw
	
	mov ax, 0200h			; Set Cursor
	xor bx, bx
	mov dx, 00D0Bh
	int 10h
	
	mov ah, 0eh				; Print winner digit
	mov al, byte[GameWinner]
	add al, '0'
	mov bx, 00030h
	int 10h
	
	mov dx, TextWinnerSnake	; Print winner text
	mov ax, 0900h
	int 21h
	
	jmp .EndGameOver
	
.PrintRoundDraw:
	mov ax, 0200h			; Set Cursor
	xor bx, bx
	mov dx, 00D0Eh
	int 10h
	
	mov dx, TextRoundDraw	; Print round draw text
	mov ax, 0900h
	int 21h
	
.EndGameOver:
	mov [GameState], GameStateExit
	
	jmp .Exit
	
	; Game Tick
.GameLoop:
	; LOCAL VARS:
	push snake1				; bp-2
	call GetNextPoint
	push snake2				; bp-4
	call GetNextPoint
	
	; Local:
	; 	bp-6 - collide snake1
	;	bp-8 - collide snake2
	;	bp-9 - pickup snake1
	;	bp-10 - pickup snake2
	sub sp, 6

.CheckPickUpBonus:
	;	bp-9 - pickup snake1
	;	bp-10 - pickup snake2
	mov word[bp-10], 0			; Init zero
	
	mov ax, [BonusXY]
	
	cmp [bp-2], ax			; Snake1
	jnz @f
	mov byte[bp-9], 1
	inc [Player1Score]
@@:
	cmp [bp-4], ax			; Snake2
	jnz @f
	mov byte[bp-10], 1
	inc [Player2Score]
@@:
	; If bonus is pickup
	mov ax, [bp-10]
	or ax, ax
	jz @f
	
	call UpdateScore
	call SpawnBonus
	jmp .CheckPickUpBonus_After
@@:
	call DrawBonus
.CheckPickUpBonus_After:

.MoveSnakesToNextPoints:
	xor ax, ax
	
	mov al, [bp-9]
	push ax
	push word[bp-2]
	push snake1
	call MoveSnake
	
	mov al, [bp-10]
	push ax
	push word[bp-4]
	push snake2
	call MoveSnake
	
	;jmp .Exit
.Collisions:
.CollisionsHeadByHead:
	mov ax, [bp-2]
	cmp ax, [bp-4]		; compare next points
	jnz .CollideSnakes
	
	; GameOver
	mov [GameOver], 1
	mov [GameWinner], 0
	mov ax, [bp-2]
	push BMP_SnakeDead
	call DrawCell
	mov word[bp-6], 1
	mov word[bp-8], 1
	jmp .AfterCollisions	; Draw round
	
.CollideSnakes:
	xor ax, ax
	
	push word[bp-2]					; Check Point of snake1
	push snake1
	call CollidePointSnakeWithoutHead
	
	push word[bp-2]
	push snake2
	call CollidePointSnakeWithoutHead
	
	or ax, ax
	jz .CheckPointOfSnake2		; if collide = 0
	; else
	; GameOver
	mov [GameOver], 1
	mov [GameWinner], 2
	mov ax, [bp-2]
	push BMP_SnakeDead
	call DrawCell
	mov word[bp-6], 1

.CheckPointOfSnake2:
	xor ax, ax
	
	push word[bp-4]					; Check Point of snake2
	push snake1
	call CollidePointSnakeWithoutHead
	
	push word[bp-4]
	push snake2
	call CollidePointSnakeWithoutHead
	
	or ax, ax
	jz .AfterCollisions		; if collide = 0
	; else
	; GameOver
	mov [GameOver], 1
	
	cmp [GameWinner], 2	; To je .Draw
	mov [GameWinner], 0	; SetDraw
	je .Draw
	mov [GameWinner], 1 ; UnsetDraw
.Draw:
	mov ax, [bp-4]
	push BMP_SnakeDead
	call DrawCell
	mov word[bp-8], 1
	
.AfterCollisions:
	
.Exit:
	mov sp, bp
	pop bp
	popa
	
	; Return to DOS
	pushf  
	call dword [cs:Old8]
	iret
	
; void
SpawnBonus:
	push ax, bx, dx, cx
	
	call Random_Next		; ax - random
	xor dx, dx
	mov bx, FieldW*FieldH
	div bx
	
	mov ax, dx
	xor dx, dx
	
	mov bx, FieldW
	div bx					; dx - X, ax - Y
	
	; TODO
	mov ah, dl
	
	mov [BonusXY], ax
	
	; Sprite
	call Random_Next
	test ax, 4
	jz .B_Banan
	
	mov [BonusID], 0
	jmp .Draw
	
.B_Banan:
	mov [BonusID], 1
	
.Draw:
	; Draw bonus
	call DrawBonus
	
.Exit:
	pop cx, dx, bx, ax
	ret
	
DrawBonus:
	push ax
	
	cmp [BonusID], 1
	jz .B_Banan
	
	mov ax, [BonusXY]
	push BMP_BonusApple
	call DrawCell
	jmp .Exit
	
.B_Banan:
	mov ax, [BonusXY]
	push BMP_BonusBanan
	call DrawCell
	jmp .Exit
	
.Exit:
	pop ax
	ret

; Input:
;	AH - X, AL - Y
;	STACK:	CSnake
SetSnakeDir:
	push bp
	mov bp, sp
	pusha
	
	mov bx, [bp+4]
	
	xor cx, cx
	
	mov dh, [Snake.oldDX]
	mov dl, [Snake.oldDY]
	
	add dh, ah
	or cl, dh
	add dl, al
	or cl, dl
	
	jz .Exit	; if NewDir + OldDir = 0, if snake want reverse direction
	
	mov [Snake.dirX], ah
	mov [Snake.dirY], al
	
.Exit:
	
	popa
	leave
	ret

StartGameLoop:
	pusha
	
	; Init vars
	mov [GameOver], 0
	mov [GameWinner], 0
	
	; Change int vector to DOS Timer
	mov dx, TimerTick
	mov ax, 2508h		; 25 - set int vector, 08 - timer
	int 21h
	
	; SetVideoMode
	call SetVideoMode
	
	; Fill video screen
	push snake1
	call RenderSnake
	push snake2
	call RenderSnake
	
	call SpawnBonus
	
	call ShowScoresText
	mov ax, FieldOffsetU
	dec ax
	call DrawLine
	
	; Handle keys
.ReadKey:
	xor ax, ax
	int 16h
	
	cmp		al, 1Bh ; if key == ESC
	jz		.CloseGame
	
	; For snake1
	push snake1
	
.CheckUp:
	cmp		ah, 48h
	jnz		.CheckDn
	mov		ax, 000FFh
	call	SetSnakeDir
	jmp		.EndReadSnakeKey
.CheckDn:
	cmp		ah, 50h
	jnz		.CheckL
	mov		ax, 00001h
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey
.CheckL:
	cmp		ah,4Bh
	jnz		.CheckR
	mov		ax, 0FF00h
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey
.CheckR:
	cmp		ah,4Dh
	jnz		.CheckSnake2Keys
	mov		ax, 00100h
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey
	
	; Snake2 keys
.CheckSnake2Keys:
	add sp, 2
	push snake2
	
.CheckU2:
	cmp		al, 'w'
	jnz		.CheckD2
	mov		ax, 000FFh
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey
.CheckD2:
	cmp		al, 's'
	jnz		.CheckL2
	mov		ax, 00001h
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey
.CheckL2:
	cmp		ah, 1Eh
	jnz		.CheckR2
	mov		ax, 0FF00h
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey
.CheckR2:
	cmp		ah, 20h
	jnz		.EndReadSnakeKey
	mov		ax, 00100h
	call 	SetSnakeDir
	jmp		.EndReadSnakeKey

.CheckElse:
	jmp		.ReadKey
	
.EndReadSnakeKey:
	add		sp, 2
	jmp		.ReadKey
	
.CloseGame:
	mov [GameState], GameStateExit

	popa
	ret

ShowGameMenu:
	push bp
	mov bp, sp
	pusha
	
	call SetTxtMode
	
	mov dx, MenuHelloText	; Print text
	mov ah, 9
	int 21h
	
	xor ax, ax				; Read key
	int 16h
	
	; call StartGameLoop
	mov [GameState], GameStateGame
	
	popa
	leave
	ret
	
SetVideoMode:
	push ax
	mov ax, 13h
	int 10h
	pop ax
	ret

SetTxtMode:
	push ax
	mov ax, 3h
	int 10h
	pop ax
	ret
	
	
; ah - x
; al - y
; Stack:
;	dw PointerOfBitmask
DrawCell:
	push bp
	mov bp, sp
	pusha
	push es
	
	mov cx, VideoSegment	; Set Video segment
	mov es, cx
	
	mov cx, ax
	shr cx, 8				; CX = x
	
	; Calcuculation of the cell position
	xor ah, ah
	
	mov dx, CellSize
	mul dx
	add ax, FieldOffsetU
	mov dx, VideoW
	mul dx					; AX = VideoW * (AX(Y) * CellSize + FieldOffsetU)
	
	push ax
	mov ax, cx
	mov dx, CellSize
	mul dx
	pop cx
	add ax, cx				; AX = AX + X * CellSize
	
	mov di, ax
	mov si, [bp+4]
	
	cld						; df = 0
	
	; bx - row counter
	mov bx, CellSize
.ForRow:
	or bx, bx
	jz .Exit
	
	; dx - col counter
	mov dx, CellSize
.ForCol:
	or dx, dx
	jz .ForRowEnd

	lodsb
	stosb
	
	dec dx
	jmp .ForCol
	
.ForRowEnd:
	add di, VideoW - CellSize	; To next row

	dec bx
	jmp .ForRow
	
.Exit:
	
	
	pop es
	popa
	leave
	ret	2

Main:
	; Save int vector DOS Timer
	mov ax, 3508h
	int 21h

	mov [cs:Old8], bx
	mov [cs:Old8+2], es
	
	; Init random generator
	call Random_Init
	
	; First show GameMenu
	mov [GameState], GameStateMenu
	
.MainGameLoop:
	cmp [GameState], GameStateExit
	jz .Exit

	cmp [GameState], GameStateMenu
	jnz @f
	call ShowGameMenu
@@:
	
	cmp [GameState], GameStateGame
	jnz @f
	call StartGameLoop
@@:
	
	jmp .MainGameLoop
	
.Exit:
	; Restore int vector
	mov ds, [cs:Old8+2]
	mov dx, [cs:Old8]
	mov ax, 2508h
	int 21h
	
	mov ax, 4C00h		; Exit from program
	int	21h
	
ShowScoresText:
	pusha

	mov ax, 0200h			; Set Cursor
	xor bx, bx
	mov dx, 00101h				; YX
	int 10h
	
	mov dx, TextPlayer1	; Print winner text
	mov ax, 0900h
	int 21h
	
	mov ax, 0200h			; Set Cursor
	xor bx, bx
	mov dx, 00115h				; YX
	int 10h
	
	mov dx, TextPlayer2	; Print winner text
	mov ax, 0900h
	int 21h
	
	call UpdateScore
	
	popa
	ret
		
UpdateScore:
	push ax, bx
	
	mov ax, [Player1Score]
	mov bx, (1 + 7 + 2 + 1) + 100h
	call PrintNumber4d
	
	mov ax, [Player2Score]
	mov bx, (15h + 7 + 2 + 1) + 100h
	call PrintNumber4d
	
	pop bx, ax
	ret
	
; Input:
; 	ax - number, bh - y, bl - x
PrintNumber4d:
	push dx, cx, si, di
	
	mov cx, 4
	mov si, 10
	mov di, bx
	add di, 3	; Shift cursor on 4-1 position forward
	
@@:
	xor dx, dx
	div si
	
	push ax
	push dx
	mov ax, 0200h			; Set Cursor
	xor bx, bx
	mov dx, di				; YX
	int 10h
	pop dx
	
	mov ah, 0eh				; Print winner digit
	mov al, dl				; Char
	add al, '0'
	mov bx, 00044h			; Color
	int 10h
	
	pop ax
	
	; For end:
	dec di
	loop @b
	
	pop di, si, cx, dx
	ret
	
; al - y
DrawLine:
	pusha
	push es
	
	mov cx, VideoSegment	; Set Video segment
	mov es, cx
	
	; Calcuculation of the cell position
	mov dx, VideoW
	mul dx
	
	mov di, ax
	mov ax, 16h		; Grey color
	mov cx, VideoW

	rep stosb
	
.Exit:
	pop es
	popa
	ret

	
Random_Init:
	push ax, cx, dx

	mov ax, 2C00h
	int 21h
	mov [R_seed], dx
	
	or dx, dx
	jnz .Exit
	mov byte[R_seed], 4
	
.Exit:
	pop dx, cx, ax
	ret

Random_Next:
	push bx, dx
	
	xor dx, dx
	mov ax, [R_seed]
	mov bx, 8405h
	mul bx
	add ax, 12345
	mov [R_seed], ax
	
	pop dx, bx
	ret
	
