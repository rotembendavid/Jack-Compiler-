#Tamar Appelboim 208810507
#Rotem Ben David 315658773
#Adina's group Wednsday


#push function
#the function call the specific push function according to the type
#argument vmline=Command line in the vm file
pushf<-function(vmline,shortfilename){
    #vmline[2]=the second word in line, the type of the variable
    #vmline[3]=The third word in line, the number of offset
    switch( 
    vmline[2], 
    "constant"=push_constant(vmline[3]),
   "local"=push_local(vmline[3]),
   "argument"=push_argument(vmline[3]),
    "this"=push_this(vmline[3]),
   "that"=push_that(vmline[3]),
   "temp"=push_temp(vmline[3]),
   "pointer"=push_pointer(vmline[3]),
   "static"=push_static(vmline[3],shortfilename))
}

#pop function
#the function call the specific pop function according to the type  
#argument vmline=Command line in the vm file
popf<-function(vmline,shortfilename){
  #vmline[2]=the second word in line, the type of the variable
  #vmline[3]=The third word in line, the number of offset
  switch( 
    vmline[2], 
    "local"=pop_local(vmline[3]),
    "argument"=pop_argument(vmline[3]),
    "this"=pop_this(vmline[3]),
    "that"=pop_that(vmline[3]),
    "temp"=pop_temp(vmline[3]),
    "pointer"=pop_pointer(vmline[3]),
    "static"=pop_static(vmline[3],shortfilename))
}


#The add command's Translation 
#@SP         A=0
#A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
#D=M         D=M                   =last value in the stack (second item)
#A=A-1,      A=A-1                 =the location of the variable before the last value in the stack
#M=D+M       RAM[A]=D+RAM[A]       =save the add result in the place of the first item on the stack
                                    #this is equal to:  pop second item, pop first item
                                    #push the result of their addition to the stack.

#@SP         A=0                   after pushing the result to the stack,
                                   #we want to decrement the stack pointer
#M=M-1      RAM[A]=RAM[A]-1        #decrement the stack pointer

#add function
#argument vmline=Command line in the vm file
addf<-function(vmline){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","M=D+M","@SP","M=M-1"),dest)
}


#-------------------------------------------------------------------------------------------------------------------------#

"The sub command's Translation 
@SP         A=0
A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
D=M         D=M                   =last value in the stack (second item)
A=A-1,      A=A-1                 =the location of the variable before the last value in the stack
M=M-D       RAM[A]=RAM[A]-D       =save the sub result in the place of the first item on the stack
                                   this is equal to:  pop second item, pop first item
                                   push the result of their substraction to the stack.

@SP         A=0                   after pushing the result to the stack,
                                   we want to decrement the stack pointer
M=M-1      RAM[A]=RAM[A]-1        decrement the stack pointer"

#sub function
#argument vmline=Command line in the vm file
subf<-function(vmline){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","M=M-D","@SP","M=M-1"),dest)
}

#---------------------------------------------------------------------------------------------------------------------------#

"The eq command's Translation 
@SP         A=0
A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
D=M         D=M                   =last value in the stack (second item)
A=A-1,      A=A-1                 =the location of the variable before the last value in the stack
D=D-M       D=D-RAM[A]            =save the sub result of the second item minus first item
@IF_TRUEnum                        =loads to A the address of the label IF_TRUEnum
D;JEQ                             =if the subtraction of the two items is 0-jumps to label IF_TRUEnum 
D=0                                else - the subtraction of the two items is not 0 -then push o to the stack
@SP                                
A=M-1                             the location of the last value in the stack
A=A-1                             =the location of the variable before the last value in the stack
M=D                               push o to the stack
@IF_FALSEnum                      loads to A the address of the label IF_FALSEnum
0;JMP                             jumps to label IF_FALSEnum 
(IF_TRUEnum)                       label IF_TRUEnum
D=-1                              the subtraction of the two items is 0- the items is equal -then push -1 to the stack
@SP
A=M-1                             the location of the last value in the stack
A=A-1                             =the location of the variable before the last value in the stack
M=D                               push -1 to the stack
(IF_FALSEnum)                     label IF_FALSEnum
@SP
M=M-1     RAM[A]=RAM[A]-1        decrement the stack pointer-
this is equal to:  pop second item, pop first item
push the result of their áomparison to the stack"

#eq function
#argument vmline=Command line in the vm file
#argument num=condition number in the file
eqf<-function(vmline,num){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","D=D-M",paste("@IF_TRUE",num,sep=""),"D;JEQ","D=0","@SP","A=M-1","A=A-1","M=D",paste("@IF_FALSE",num,sep=""),"0;JMP",paste("(IF_TRUE",num,")",sep=""),"D=-1","@SP","A=M-1","A=A-1","M=D",paste("(IF_FALSE",num,")",sep=""),"@SP","M=M-1"),dest)
}

#---------------------------------------------------------------------------------------------------------------------------#

"The lt command's Translation 
@SP         A=0
A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
D=M         D=M                   =last value in the stack (second item)
A=A-1,      A=A-1                 =the location of the variable before the last value in the stack
D=M-D       D=RAM[A]-D            =save the sub result of the first item minus second item
@IF_TRUEnum                        =loads to A the address of the label IF_TRUEnum
D;JLT                             =if the subtraction of the two items < 0-jumps to label IF_TRUEnum 
D=0                                else - the subtraction of the two items < 0  (first items is smaller than second item)-then push o to the stack
@SP                                
A=M-1                             the location of the last value in the stack
A=A-1                             =the location of the variable before the last value in the stack
M=D                               push o to the stack
@IF_FALSEnum                      loads to A the address of the label IF_FALSEnum
0;JMP                             jumps to label IF_FALSEnum 
(IF_TRUEnum)                       label IF_TRUEnum
D=-1                              the subtraction of the two items < 0- first items is not smaller than second item  -then push -1 to the stack
@SP
A=M-1                             the location of the last value in the stack
A=A-1                             =the location of the variable before the last value in the stack
M=D                               push -1 to the stack
(IF_FALSEnum)                     label IF_FALSEnum
@SP
M=M-1     RAM[A]=RAM[A]-1        decrement the stack pointer-

this is equal to:  pop second item, pop first item
push the result of their lt comparison to the stack"

#lt function
#argument vmline=Command line in the vm file
#argument num=condition number in the file
ltf<-function(vmline,num){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","D=M-D",paste("@IF_TRUE",num,sep=""),"D;JLT","D=0","@SP","A=M-1","A=A-1","M=D",paste("@IF_FALSE",num,sep=""),"0;JMP",paste("(IF_TRUE",num,")",sep=""),"D=-1","@SP","A=M-1","A=A-1","M=D",paste("(IF_FALSE",num,")",sep=""),"@SP","M=M-1"),dest)
}

#---------------------------------------------------------------------------------------------------------------------------#

"The gt command's Translation 
@SP         A=0
A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
D=M         D=RAM[A]                   =last value in the stack (second item)
A=A-1       A=A-1                 =the location of the variable before the last value in the stack
D=M-D       D=RAM[A]-D            =save the sub result of the first item minus second item
@IF_TRUEnum                        =loads to A the address of the label IF_TRUEnum
D;JGT                             =if the subtraction of the two items > 0-jumps to label IF_TRUEnum 
D=0                                else - the subtraction of the two items > 0  (first items is greater than second item)-then push o to the stack
@SP                                
A=M-1                             the location of the last value in the stack
A=A-1                             =the location of the variable before the last value in the stack
M=D                               push o to the stack
@IF_FALSEnum                      loads to A the address of the label IF_FALSEnum
0;JMP                             jumps to label IF_FALSEnum 
(IF_TRUEnum)                       label IF_TRUEnum
D=-1                              the subtraction of the two items < 0- first items is not greater than second item  -then push -1 to the stack
@SP
A=M-1                             the location of the last value in the stack
A=A-1                             =the location of the variable before the last value in the stack
M=D                               push -1 to the stack
(IF_FALSEnum)                     label IF_FALSEnum
@SP
M=M-1     RAM[A]=RAM[A]-1        decrement the stack pointer

this is equal to:  pop second item, pop first item
push the result of their gt comparison to the stack"

#gt function
#argument vmline=Command line in the vm file
#argument num=condition number in the file
gtf<-function(vmline,num){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","D=M-D",paste("@IF_TRUE",num,sep=""),"D;JGT","D=0","@SP","A=M-1","A=A-1","M=D",paste("@IF_FALSE",num,sep=""),"0;JMP",paste("(IF_TRUE",num,")",sep=""),"D=-1","@SP","A=M-1","A=A-1","M=D",paste("(IF_FALSE",num,")",sep=""),"@SP","M=M-1"),dest)
}

#---------------------------------------------------------------------------------------------------------------------------#

"
The neg/not command's Translation
@SP                   
A=M-1         A=RAM[A]-1            =the location of the last value in the stack 
M=!M          RAM[A]=!RAM[A]          enter the value in the negative
"

#neg function
negf<-function(){
  writeLines(c("@SP","A=M-1","M=!M"),dest)
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

"The and command's Translation 
@SP         A=0
A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
D=M         D=RAM[A]              =last value in the stack (second item)
A=A-1       A=A-1                 =the location of the variable before the last value in the stack
M=D&M       RAM[A]=RAM[A]&D       calculating the AND between the two items in stack, and save the result in stack
@SP
M=M-1       RAM[A]=RAM[A]-1        decrement the stack pointer

this is equal to:  pop second item, pop first item
push the result of their AND to the stack"

#and function
andf<-function(){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","M=D&M","@SP","M=M-1"),dest)
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

"The or command's Translation 
@SP         A=0
A=M-1       A=RAM[A]-1            =the location of the last value in the stack 
D=M         D=RAM[A]              =last value in the stack (second item)
A=A-1       A=A-1                 =the location of the variable before the last value in the stack
M=D|M       RAM[A]=RAM[A]|D       calculating the OR between the two items in stack, and save the result in stack
@SP
M=M-1       RAM[A]=RAM[A]-1        decrement the stack pointer

this is equal to:  pop second item, pop first item
push the result of their OR to the stack"

#or function
orf<-function(){
  writeLines(c("@SP","A=M-1","D=M","A=A-1","M=D|M","@SP","M=M-1"),dest)
}

#-----------------------------------------------push functions--------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#

"
@value        A=value          =num contant  
D=A                            D=num contant
@SP           A=0              SP is Stack Pointer: points to the top of the stack 
A=M           A=RAM[A]         A=the top of stack addres (the empty place in stack)
M=D           RAM[A]=D         push the constant to the top of the stack
@SP           A=0              after pushing constant to the stack, we want to increment the stack pointer
M=M+1         RAM[A]=RAM[A]+1  so now the stack pointer points to the next free location of the stack
"

push_constant<-function(const_num){
  writeLines(c(paste("@",const_num,sep = ""),"D=A","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#----------------------------------------------------------------------------------------------------------------------#

"
@offset   A=offset            =the offset from the beginning of LCL area
D=A                           D=offset
@LCL      A=1                 =points to the addres of the  beginning LCL area
A=M+D     A=RAM[A]+D          =addition of the LCL beginning adress with the offset - points to the value we want
D=M       D=RAM[A]            D=the value we want to push to the stack
@SP       A=0                 Stack Pointer: points to the top of the stack
A=M       A=RAM[A]            A=the top of stack addres (the empty place in stack)
M=D       RAM[A]=D            push the value to the top of the stack
@SP       A=0                 after pushing local number to the stack, we want to increment the stack pointer
M=M+1     RAM[A]=RAM[A]+1     so now the stack pointer points to the next free location of the stack

"
push_local<-function(num_local){
  offset=paste("@",num_local,sep = "")
  writeLines(c(offset,"D=A","@LCL","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#----------------------------------------------------------------------------------------------------------------------#

"
@offset   A=offset            =the offset from the beginning of ARG area
D=A                           D=offset
@ARG      A=1                 =points to the addres of the  beginning ARG area
A=M+D     A=RAM[A]+D          =addition of the ARG beginning adress with the offset - points to the value we want
D=M       D=RAM[A]            D=the value we want to push to the stack
@SP       A=0                 Stack Pointer: points to the top of the stack
A=M       A=RAM[A]            A=the top of stack addres (the empty place in stack)
M=D       RAM[A]=D            push the value to the top of the stack
@SP       A=0                 after pushing argument number to the stack, we want to increment the stack pointer
M=M+1     RAM[A]=RAM[A]+1     so now the stack pointer points to the next free location of the stack
"

push_argument<-function(num_argument){
  writeLines(c(paste("@",num_argument,sep = ""),"D=A","@ARG","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#------------------------------------------------------------------------------------------------------------------------#

"
@offset   A=offset            =the offset from the beginning of THIS area
D=A                           D=offset
@THIS     A=3                 =points to the addres of the  beginning THIS area
A=M+D     A=RAM[A]+D          =addition of the THIS beginning adress with the offset - points to the value we want
D=M       D=RAM[A]            D=the value we want to push to the stack
@SP       A=0                 Stack Pointer: points to the top of the stack
A=M       A=RAM[A]            A=the top of stack addres (the empty place in stack)
M=D       RAM[A]=D            push the value to the top of the stack
@SP       A=0                 after pushing THIS number to the stack, we want to increment the stack pointer
M=M+1     RAM[A]=RAM[A]+1     so now the stack pointer points to the next free location of the stack
"

push_this<- function(num_this) {
  writeLines(c(paste("@",num_this,sep = ""),"D=A","@THIS","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#------------------------------------------------------------------------------------------------------------------------#

"
@offset   A=offset            =the offset from the beginning of THAT area
D=A                           D=offset
@THAT     A=4                =points to the addres of the  beginning THAT area
A=M+D     A=RAM[A]+D          =addition of the THAT beginning adress with the offset - points to the value we want
D=M       D=RAM[A]            D=the value we want to push to the stack
@SP       A=0                 Stack Pointer: points to the top of the stack
A=M       A=RAM[A]            A=the top of stack addres (the empty place in stack)
M=D       RAM[A]=D            push the value to the top of the stack
@SP       A=0                 after pushing That value to the stack, we want to increment the stack pointer
M=M+1     RAM[A]=RAM[A]+1     so now the stack pointer points to the next free location of the stack
"
push_that<- function(num_that) {
  writeLines(c(paste("@",num_that,sep = ""),"D=A","@THAT","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#-------------------------------------------------------------------------------------------------------------------------#

"
@offset_temp      A=5+offset            A=the place in the ram we want according to the offset        
D=M               D=RAM[A]              D=the value in the temp we want
@SP               A=0                   Stack Pointer: points to the top of the stack
A=M               A=RAM[A]              A=the top of stack addres (the empty place in stack)
M=D               RAM[A]=D              push the value to the top of the stack
@SP               A=0                   after pushing the value to the stack, we want to increment the stack pointer
M=M+1             RAM[A]=RAM[A]+1       so now the stack pointer points to the next free location of the stack
"
push_temp<- function(num_temp) {
  #calculate  num temp + 5= the place in Ram we want
  offset_ram=5+as.integer(num_temp)
  #cocat @ to the offset_ram
  offset_temp=paste("@",offset_ram,sep = "")
  writeLines(c(offset_temp,"D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#-------------------------------------------------------------------------------------------------------------------------#
"
@THIS /@THAT         A=3 /A=4            if num pointer=0 loads THIS addres and if num pointer=1 loads THAT addres
D=M                  D=RAM[A]            D=the addres of the beginnig THIS/THAT area
@SP                  @0                  Stack Pointer: points to the top of the stack
A=M                  A=RAM[A]            A=the top of stack addres (the empty place in stack)
M=D                  RAM[A]=D            push the value to the top of the stack
@SP                  A=0                 after pushing the value to the stack, we want to increment the stack pointer
M=M+1                RAM[A]=RAM[A]+1     so now the stack pointer points to the next free location of the stack
"
push_pointer<-function(num_pointer){
  #this - num_pointer=0
  if(as.integer(num_pointer)==0)
    writeLines(c("@THIS","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
  #that -  num_pointer=1
  else
    writeLines(c("@THAT","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}

#--------------------------------------------------------------------------------------------------------------------------#

"
@filename.num_static     A=Value address        the address of the static  area in this file plus the offset(num static)
@D=M                     D=RAM[A]               D=the value in the static we want
@SP                      A=0                    Stack Pointer: points to the top of the stack
A=M                      A=RAM[A]               A=the top of stack addres (the empty place in stack)
M=D                      RAM[A]=D               push the value to the top of the stack
@SP                      A=0                    after pushing the value to the stack, we want to increment the stack pointer
M=M+1                    RAM[A]=RAM[A]+1        so now the stack pointer points to the next free location of the stack
"

push_static<-function(num_static,filename){
  #cocat @ to the filename and num_static
  myfilename=paste("@",filename,".",num_static,sep = "")
  writeLines(c(myfilename,"D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
}


#---------------------------------------------------pop functions-----------------------------------------------------------#


"
@offset_local    A=offset         
D=A              D=A                save in D the local offset
@LCL             A=1                points to the addres of the  beginning LCL area
D=D+M            D=D+RAM[A]         addition of the LCL beginning adress with the offset - points to the place we want to insert the value from the stack
@R13             A=13               free memory location for the programmer to use
M=D              RAM[A]=D           RAM[13]=the place we want to insert the value in the LCL area
@SP              A=0                Stack Pointer: points to the top of the stack
A=M-1            A=RAM[A]-1         A=points to the address of the value in top of stack
D=M              D=RAM[A]           D=value in top of stack
@SP              A=0                after popping the value from the stack, we want to decrease the stack pointer
M=M-1            RAM[A]=RAM[A]-1    so now the stack pointer points to the next free location of the stack
@R13             A=13               =the address of the place we want to insert the value in the LCL area
A=M              A=RAM[A]           A=the place we want to insert the value in the LCL area
M=D              RAM[A]=D           insert the value to the LCL place we want
"


pop_local<-function(num_local){
offset_local=paste("@",num_local,sep = "")
writeLines(c(offset_local,"D=A","@LCL","D=D+M","@R13","M=D","@SP","A=M-1","D=M","@SP","M=M-1","@R13","A=M","M=D"),dest)
}
#---------------------------------------------------------------------------------------------------------------------------#
"
@offset_argument A=offset         
D=A              D=A                save in D the argument offset
@LCL             A=1                points to the addres of the  beginning ARG area
D=D+M            D=D+RAM[A]         addition of the ARG beginning adress with the offset - points to the place we want to insert the value from the stack
@R13             A=13               free memory location for the programmer to use
M=D              RAM[A]=D           RAM[13]=the place we want to insert the value in the ARG area
@SP              A=0                Stack Pointer: points to the top of the stack
A=M-1            A=RAM[A]-1         A=points to the address of the value in top of stack
D=M              D=RAM[A]           D=value in top of stack
@SP              A=0                after popping the value from the stack, we want to decrease the stack pointer
M=M-1            RAM[A]=RAM[A]-1    so now the stack pointer points to the next free location of the stack
@R13             A=13               =the address of the place we want to insert the value in the ARG area
A=M              A=RAM[A]           A=the place we want to insert the value in the ARG area
M=D              RAM[A]=D           insert the value to the ARG place we want
"

pop_argument<-function(num_argument){
  offset_argument=paste("@",num_argument,sep = "")
  writeLines(c(offset_argument,"D=A","@ARG","D=D+M","@R13","M=D","@SP","A=M-1","D=M","@SP","M=M-1","@R13","A=M","M=D"),dest)
}
#------------------------------------------------------------------------------------------------------------------------------#
"
@offset_this     A=offset         
D=A              D=A                save in D the THIS offset
@THIS            A=1                points to the addres of the  beginning THIS area
D=D+M            D=D+RAM[A]         addition of the THIS beginning adress with the offset - points to the place we want to insert the value from the stack
@R13             A=13               free memory location for the programmer to use
M=D              RAM[A]=D           RAM[13]=the place we want to insert the value in the THIS area
@SP              A=0                Stack Pointer: points to the top of the stack
A=M-1            A=RAM[A]-1         A=points to the address of the value in top of stack
D=M              D=RAM[A]           D=value in top of stack
@SP              A=0                after popping the value from the stack, we want to decrease the stack pointer
M=M-1            RAM[A]=RAM[A]-1    so now the stack pointer points to the next free location of the stack
@R13             A=13               =the address of the place we want to insert the value in the THIS area
A=M              A=RAM[A]           A=the place we want to insert the value in the THIS area
M=D              RAM[A]=D           insert the value to the THIS place we want
"

pop_this<-function(num_this){
  offset_this=paste("@",num_this,sep = "")
  writeLines(c(offset_this,"D=A","@THIS","D=D+M","@R13","M=D","@SP","A=M-1","D=M","@SP","M=M-1","@R13","A=M","M=D"),dest)
}

#----------------------------------------------------------------------------------------------------------------------------#

"
@offset_this     A=offset         
D=A              D=A                save in D the THAT offset
@THAT            A=1                points to the addres of the  beginning THAT area
D=D+M            D=D+RAM[A]         addition of the THAT beginning adress with the offset - points to the place we want to insert the value from the stack
@R13             A=13               free memory location for the programmer to use
M=D              RAM[A]=D           RAM[13]=the place we want to insert the value in the THAT area
@SP              A=0                Stack Pointer: points to the top of the stack
A=M-1            A=RAM[A]-1         A=points to the address of the value in top of stack
D=M              D=RAM[A]           D=value in top of stack
@SP              A=0                after popping the value from the stack, we want to decrease the stack pointer
M=M-1            RAM[A]=RAM[A]-1    so now the stack pointer points to the next free location of the stack
@R13             A=13               =the address of the place we want to insert the value in the THAT area
A=M              A=RAM[A]           A=the place we want to insert the value in the THAT area
M=D              RAM[A]=D           insert the value to the THAT place we want
"
pop_that<-function(num_that){
  offset_that=paste("@",num_that,sep = "")
  writeLines(c(offset_that,"D=A","@THAT","D=D+M","@R13","M=D","@SP","A=M-1","D=M","@SP","M=M-1","@R13","A=M","M=D"),dest)
}
#----------------------------------------------------------------------------------------------------------------------------#
"
@offset_temp      A=5+offset        A=the place in the ram we want to insert the value from the stack, according to the offset        
D=A                                 save the offset of temp from ram
@R13             A=13               free memory location for the programmer to use
M=D              RAM[A]=D           RAM[13]=the place we want to insert the value in the temp area
@SP              A=0                Stack Pointer: points to the top of the stack
A=M-1            A=RAM[A]-1         A=points to the address of the value in top of stack
D=M              D=RAM[A]           D=value in top of stack
@SP              A=0                after popping the value from the stack, we want to decrease the stack pointer
M=M-1            RAM[A]=RAM[A]-1    so now the stack pointer points to the next free location of the stack
@R13             A=13               =the address of the place we want to insert the value in the temp area
A=M              A=RAM[A]           A=the place we want to insert the value in the temp area
M=D              RAM[A]=D           insert the value to the temp place we want
"
pop_temp<-function(num_temp){
  #calculate  num temp + 5= the place in Ram we want
  offset_ram=5+as.integer(num_temp)
  #cocat @ to the offset_ram
  offset_temp=paste("@",offset_ram,sep = "")
  writeLines(c(offset_temp,"D=A","@R13","M=D","@SP","A=M-1","D=M","@SP","M=M-1","@R13","A=M","M=D"),dest)
}


#------------------------------------------------------------------------------------------------------------------------------#

"
@SP          A=0                 Stack Pointer: points to the top of the stack
A=M-1        A=RAM[A]-1          A=points to the address of the value in top of stack
D=M          D=RAM[A]            D=value in top of stack
@THIS/@THAT  A=3 /A=4            if num pointer=0 loads THIS addres and if num pointer=1 loads THAT addres
M=D          RAM[A]=D            insert the value to the  place we want
@SP          A=0                 after popping the value from the stack, we want to decrease the stack pointer
M=M-1        RAM[A]=RAM[A]-1     so now the stack pointer points to the next free location of the stack
"
pop_pointer<-function(num_pointer){
  #this
  if(as.integer(num_pointer)==0)
    writeLines(c("@SP","A=M-1","D=M","@THIS","M=D","@SP","M=M-1"),dest)
  #that
  else
    writeLines(c("@SP","A=M-1","D=M","@THAT","M=D","@SP","M=M-1"),dest)
}


#-------------------------------------------------------------------------------------------------------------------------------#
"
@SP          A=0                 Stack Pointer: points to the top of the stack
A=M-1        A=RAM[A]-1          A=points to the address of the value in top of stack
D=M          D=RAM[A]            D=value in top of stack
@filename.num_static     A=Value address        the address of the static  area in this file plus the offset(num static)
M=D          RAM[A]=D            insert the value to the  place we want
@SP          A=0                 after popping the value from the stack, we want to decrease the stack pointer
M=M-1        RAM[A]=RAM[A]-1     so now the stack pointer points to the next free location of the stack
"
pop_static<-function(num_static,filename){
  myfilename=paste("@",filename,".",num_static,sep = "")
  writeLines(c("@SP","A=M-1","D=M",myfilename,"M=D","@SP","M=M-1"),dest)
}



#-------------------------------------------------------------------------------------------------------------------------------#

"
(myfileName.labelName)         
"

#label command
#creating a refrence point in the program as follows:currentFileName.labelName
labelf<-function(vmline,myfileName){
  labelName=vmline[2]
  setlabel=paste("(",myfileName,".",labelName,")",sep = "")
  writeLines(setlabel,dest)
}

#--------------------------------------------------------------------------------------------------------------------------------#
"
@myfileName.labelname     A=label adress     loading the adress of the label to A
0;JMP                                        jumping to the adress in A without condition
"
#jumping to a label

gotof<-function(vmline,myfileName){
  labelName=vmline[2]
  jumplabel=paste("@",myfileName,".",labelName,sep = "")
  writeLines(c(jumplabel,"0;JMP"),dest)
}


#--------------------------------------------------------------------------------------------------------------------------------#
"
@SP                 A=0                                Stack Pointer: points to the top of the stack
M=M-1               Ram[A]=Ram[A]-1                    A=points to the address of the value in top of stack
A=M                 A=Ram[A]                           A=value in top of stack
D=M                 D=Ram[A]                           D=value in top of stack
@jumplabel          A=label adress                     A=label adress 
D;JNE               if D!=0 jump to the label          if D!=0 jump to the adress in A
"
#ifgoto command

ifgotof<-function(vmline,myfileName){
  labelName=vmline[2]
  jumplabel=paste("@",myfileName,".",labelName,sep = "")
  #  writeLines(c("@SP","M=M-1","A=M","D=M",jumplabel,"D;JNE"),dest)
  writeLines(c("@SP","M=M-1","D=M",jumplabel,"D;JNE"),dest)
}

#--------------------------------------------------------------------------------------------------------------------------------#


functionf<-function(vmline){
  #vmline[2] is the name of the funcion
  functionName=vmline[2]
  #k=num of local variables function
  k=as.integer(vmline[3])
  functionLabel=paste("(",functionName,")",sep = "")
  writeLines(c(functionLabel),dest)
  #writeLines(c(paste('@',k,sep = ""),"D=A","@f.End","D;JEQ","(f.Loop)","@SP","A=M","M=0","@SP","M=M+1","@f.Loop","D=D-1;JNE","(f.End)"),dest)
  #additional way to push o contants
  #if k=0 there are no local variables in function
  if(k!=0){
    #loop until k-1 because its beginning from 0
    for(i in seq(0,k-1)){
      push_constant(0)
    }
  }
}

#--------------------------------------------------------------------------------------------------------------------------------#
"
//push return address
@ funcNme.ReturnAddressNumret         a=return address    loads the return address to A
D=A                                   D=return address    loads the return address to D
@SP                                   A=0                 A=the location in the ram that save the stack top address              
A=M                                   A=Ram[A]            the stack top address
M=D                                   Ram[A]=D            saves the return address in top of stack
@SP                                   A=SP                A=the current top of stack
M=M+1                                 Ram[A]=Ram[A]+1     now the stack pointer points to the next free location of the stack  

// push LCL
@LCL                                  A=1                            loads the pointer of LCL address to A  
D=M                                   D=Ram[A]=Ram[1]                D=current address of LCL
@SP                                   A=0                            A=the pointer to the top of the stack
A=M                                   A=Ram[A]=Ram[0]                A=address of the tpo of the stack
M=D                                   Ram[A]=D                       push the current LCL address to the top of stack       
@SP                                   A=0                            A=pointer to the address of the top of the stack
M=M+1                                 Ram[A]=Ram[A]+1                now the stack pointer points to the next free location of the stack 

// push ARG
@ARG                                  A=2                            loads the pointer of ARG address to A  
D=M                                   D=Ram[A]=Ram[2]                loads the addres of ARG to D
@SP                                   A=0                            A=pointer to the address of the top of the stack  
A=M                                   A=Ram[A]=Ram[0]                A=top of the stack
M=D                                   Ram[0]=ARG address             loads the address of Arg to the top of the stack
@SP                                   A=0                            loads the pointer of ARG address to A 
M=M+1                                 Ram[A]=Ram[A]+1                now the stack pointer points to the next free location of the stack 

//push THIS
@THIS                                 A=3                            loads the pointer of THIS address to A  
D=M                                   D=Ram[A]=Ram[3]                loads the addres of THIS to D
@SP                                   A=0                            A=pointer to the address of the top of the stack  
A=M                                   A=Ram[A]=Ram[0]                A=top of the stack
M=D                                   Ram[0]=THIS address            loads the address of THIS to the top of the stack
@SP                                   A=0                            loads the pointer of THIS address to A 
M=M+1                                 Ram[A]=Ram[A]+1                now the stack pointer points to the next free location of the stack 

//push THAT
@THAT                                 A=4                            loads the pointer of THAT address to A  
D=M                                   D=Ram[A]=Ram[4]                loads the addres of THAT to D
@SP                                   A=0                            A=pointer to the address of the top of the stack  
A=M                                   A=Ram[A]=Ram[0]                A=top of the stack
M=D                                   Ram[0]=THIS address            loads the address of THAT to the top of the stack
@SP                                   A=0                            loads the pointer of THAT address to A 
M=M+1                                 Ram[A]=Ram[A]+1                now the stack pointer points to the next free location of the stack 


//ARG = SP-n-5 
@SP                                  A=0                                    loads the pointer of THAT address to A
D=M                                  D=Ram[0]                               D=address of the top of stack
@newARG                              A=number of args+saved adresses        A=num of arguments+5
D=D-A                                Ram[0]-(num of arguments+5)            top of stack-5-arguments
@ARG                                 A=2                                    loads the pointer of ARG address to A  
M=D                                  Ram[2]=address of the top of stack     Ram[2]=address of the top of stack  


// LCL = SP
@SP                                  A=0                     loads the pointer to the top of the stack Address to A                   
D=M                                  D=Ram[0]                D=address of the top of stack
@LCL                                 A=1                     loads the pointer of LCL address to A
M=D                                  Ram[A]=D                Ram[1]=the top of the stack     Ram[1]=the address of LCL area


// goto funcName-jumping to the called func
@funcName                                    A=the address of the called function  
0; JMP                                       jumpung to the func with no condition

// label return-address
(funcName.ReturnAddress numRet)              
"
callf<-function(vmline,numRet){
  #vmline[2] is the function name
  funcName=vmline[2]
  nMinus5=as.integer(vmline[3])+5
  returnAddress=paste("@",funcName,".ReturnAddress",numRet,sep = "")
  #push return address
  writeLines(c(returnAddress,"D=A","@SP","A=M","M=D","@SP","M=M+1"),dest)
  #push LCL=1, ARG=2, THIS=3, THAT=4
  #for(i in seq(1,4)){
    #type=paste("@",i,sep = "")
    #writeLines(c(type,"D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
  #}
  writeLines(c("@LCL","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
  writeLines(c("@ARG","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
  writeLines(c("@THIS","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
  writeLines(c("@THAT","D=M","@SP","A=M","M=D","@SP","M=M+1"),dest)
  #ARG=SP-n-5
  newARG=paste("@",nMinus5,sep="")
  #writeLines(c("@SP","D=M","@5","D=D-A","@ARG","M=D"),dest)
  writeLines(c("@SP","D=M",newARG,"D=D-A","@ARG","M=D"),dest)
  #LCL=SP
  writeLines(c("@SP","D=M","@LCL","M=D"),dest)
  #goto namefunction
  writeLines(c(paste("@",funcName,sep = ""),"0;JMP"),dest)
  #label return-address
  writeLines(paste("(",funcName,".ReturnAddress",numRet,")",sep = ""),dest)
}

#--------------------------------------------------------------------------------------------------------------------------------#

"
//returnF

// FRAME = LCL
@LCL         A=1                       loads the pointer to LCL address to A
D=M          D=Ram[A]=Ram[1]           loads LCL address to D

//RET=*(FRAME-5)
//RAM[13]=(LCL-5)
@5                                     A=5
A=D-A                                  A=LCL-5
D=M                                    D=Ram[A]=Ram[5]           D =the return address     
@13                                    A=13
M=D                                    Ram[13]=return address    $13 saves the return address       

//ARG=pop()
@SP                                    A=0                        loads the pointer to the top of the stack to A
M=M-1                                  Ram[A]=Ram[A]-1         
A=M                                    A=M=Ram[A]-1
D=M                                    D=M=Ram[A]-1
@ARG                                   A=ARG ad                   loads the address of ARG to A             
A=M                                    A=Ram[A]                   
M=D                                    Ram[A]=the return adsress

//SP=ARG+1
@ARG                                   A=ARG                A=the address of ARG area
D=M                                    D=Ram[A]=Ram[ARG]    D=the first argument
@SP                                    A=sp                 A points to the top of the stack
M=D+1                                  Ram[A]=ARG[0] +1                 


//reconstruct the segments
//THAT=*(FRAME-1)
@LCL                         A=LCL                      A=the LCL address in the stack                     A points to the LCL addresss in the stack
M=M-1                        M=Ram[A]-1                 M=the address of THAT 
A=M                          A=THAT address             A=the previous address of THAT area
D=M                          D=THAT address             D=the previous address of THAT area
@THAT                        A=4
M=D                          Ram[4]=D                           Ram[4]=previous address of THAT

//THIS=*(FRAME-2)
@LCL                       A=current LCL address in the stack
M=M-1                      Ram[A]=Ram[A]-1  
A=M                        A=THIS address
D=M                        D=THIS address
@THIS                      A=3
M=D                        Ram[3]=the previous address of THIS 

//ARG=*(FRAME-3)
@LCL                       A=current LCL address 
M=M-1                      Ram[A]=Ram[A]-1
A=M                         
D=M
@ARG
M=D





"
returnf<-function(){
  #FRAME=LCL
  writeLines(c("@LCL","D=M"),dest)
  #RET=*(FRAME-5)
  #RAM[13]=(LCL-5)
  writeLines(c("@5","A=D-A","D=M","@13","M=D"),dest)
  #*ARG=pop()
  writeLines(c("@SP","M=M-1","A=M","D=M","@ARG","A=M","M=D"),dest)
  #SP=ARG+1
  writeLines(c("@ARG","D=M","@SP","M=D+1"),dest)
  #THAT=*(FRAME-1)
  writeLines(c("@LCL","M=M-1","A=M","D=M","@THAT","M=D"),dest)
  #THIS=*(FRAME-2)
  writeLines(c("@LCL","M=M-1","A=M","D=M","@THIS","M=D"),dest)
  #ARG=*(FRAME-3)
  writeLines(c("@LCL","M=M-1","A=M","D=M","@ARG","M=D"),dest)
  #LCL=*(FRAME-4)
  writeLines(c("@LCL","M=M-1","A=M","D=M","@LCL","M=D"),dest)
  #goto RET
  writeLines(c("@13","A=M","0;JMP"),dest)
}
#--------------------------------------------------------------------------------------------------------------------------------#
#inializes  the adress of stack 
beginp<-function(){
  writeLines(c("@256","D=A","@SP","M=D"),dest)
  #writeLines(c("@Sys.init","0;JMP"),dest)
  
  callSys<-unlist(strsplit('call Sys.init 0'," "))
  callf(callSys,0)
}

#-----------------------------------------------main program---------------------------------------------------------------------#  

setwd("C:/R/tar2/targ2")
#dirs_list=list all  dirs in the folder
dirs_list<-list.dirs(path = "C:\\R\\tar2\\targ2", full.names = TRUE, recursive = FALSE)
#loop over the dirs
for (currentDir in dirs_list){
# set the current working dir
setwd(currentDir)
#extracts the dir name and appends the extension "asm"
destfileName=paste(basename(currentDir),".asm",sep="")   
#open new asm file to write
dest<-file(destfileName,"w")  
#vm_files_list=list all vm files in the dir
vm_files_list<-list.files(path = currentDir,pattern="vm")
#if there is more then 1 vm files- writes the bootstarp commands
if(length(vm_files_list)>1)
  #call the function that write the bootstarp
  beginp()
#return addres counter
numRet<-1
#label counter
num<-0
#loop over the vm files list in the current dir
for (i in 1:length(vm_files_list)){
   #src=open the current vm file to read the commands
   src=file(vm_files_list[i],'r')
   #extracts the file name without the extension
   shortfilename=sub("^([^.]*).*", "\\1", vm_files_list[i]) 
   #fc=list all lines(commands) in the current vm file
   fc <- readLines(src, -1)
     #loop over the commands
     for (i in 1:length(fc)){
     #sentence=current command
     sentence<-fc[i]
     #words=list of the words in the sentence
     words<-unlist(strsplit(sentence," "))
     #words[1]= the first word in line= type of the command
     #the switch block calls the function according to the type of command
     x = switch(
     words[1], 
      "\\"=break, 
      "push"=pushf(words,shortfilename), 
      "pop"=popf(words,shortfilename),
      "add"=addf(words),
      "sub"=subf(words),
      "eq"={eqf(words,num)
          num<-num+1},
      "lt"={ltf(words,num)
          num<-num+1},
      "gt"={gtf(words,num)
           num<-num+1},
      "neg","not"=negf(),
      "and"=andf(),
      "or"=orf(),
      "label"=labelf(words,shortfilename),
      "goto"=gotof(words,shortfilename),
     "if-goto"=ifgotof(words,shortfilename),
     "function"=functionf(words),
     "call"={callf(words,numRet)
             numRet<-numRet+1},
     "return"=returnf()
    ) 
     }
   #close src file
   close(src);
}
# close dest file
 close(dest)
}
