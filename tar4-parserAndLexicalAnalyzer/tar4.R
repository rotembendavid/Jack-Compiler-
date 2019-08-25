install.packages("<the package's name>")

#keywords list
keywords_list=list('class','constructor','function','method','field','static','var','int','char','boolean','void','true','false','null','this','let','do','if','else','while','return')
#symbols list
symbols_list=list('{','}','(',')','[',']','.',',',';','+','-','*','/','&','|','<','>','=','~')
#op list
op_list=list('+','-','*','/','&','|','<','>','=','&lt;','&gt;','&amp;')

isDigit<-function(first_letter){
  return (!(grepl("^[A-Za-z]+$", first_letter, perl = T)))
  
}

#the function get type and value and write new token according to it
write_token<-function(token_kind,token,dest){
  #print(token_kind)
  #if token kind is "symbol" ,calls to function that writes symbol tokens
  if(token_kind=="symbol"){
    write_symbol_token(token,dest)
  }
  else{
    #make token line
    token_line=paste("  ","<",token_kind,"> ",token," </",token_kind,">",sep="")
    #writes the token to the token's file
    writeLines(token_line,dest)
  }
}

#the function get list and element and checks if the element is in the list
check_element_in_list<-function(element,current_list){
  for (current in current_list){
    if(current==element){
      return (TRUE)
    }
  }
  return (FALSE)
}

#the function writes symbol tokens to the token's file
write_symbol_token<-function(token,dest){
  #checks if the token is '<','>','\','&' and writes the appropriate token
  if(token=="<"){
    token_write="&lt;"
  }
  else if(token==">"){
    token_write="&gt;"
  }
  else if(token=="\""){
    token_write="&quet;"
  }
  else if(token=="&"){
    token_write="&amp;"
  }
  else{
    token_write=token
  }
  token_line=paste("  ","<symbol> ",token_write," </symbol>",sep="")
  writeLines(token_line,dest)
}


get_src_without_first_line<-function(srcf){
  return (srcf[2:length(srcf)])
}

write_next_token<-function(srcf,destf){
  writeLines(srcf[1],destf)
  return(get_src_without_first_line(srcf))
}
  
get_word_from_line<-function(word_num,line){
  line=trimws(line, "l")
  words<-unlist(strsplit(line," "))
  return(words[word_num])
}

classNamef<-function(srcf,destf){
 # writeLines("<className>",destf)
  #write identifier>    </identifier>
  srcf<-write_next_token(srcf,destf)
 # writeLines("</className>",destf)
  return (srcf)
}

varNamef<-function(srcf,destf){
  #writeLines("<varName>",destf)
  #write identifier>    </identifier>
  srcf<-write_next_token(srcf,destf)
  return(srcf)
}

typef<-function(srcf,destf){
  #writeLines("<type>",destf)
  word2=get_word_from_line(2,srcf[1])
  if((word2=="int")||(word2=="char")||(word2=="boolean")){
   srcf<-write_next_token(srcf,destf)
    return(srcf)
  }
  else{
    srcf<-classNamef(srcf,destf)
    #writeLines("</type>",destf)
    #return (srcf)
  }
  return (srcf)
}

classVarDec<-function(srcf,destf){
  writeLines("<classVarDec>",destf)
  #write identifier> static/field   </identifier>
  srcf<-write_next_token(srcf,destf)
  srcf<-typef(srcf,destf)
  srcf<-varNamef(srcf,destf)
  repeat{
    word2=get_word_from_line(2,srcf[1])
    if(word2!=","){
      break
    }
    else{
      #write <symbol> , </symbol>
      srcf<-write_next_token(srcf,destf)
      srcf<-varNamef(srcf,destf)
    }
  }
  #write <symbol> ; </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</classVarDec>",destf)
  return(srcf)
}


subroutineNamef<-function(srcf,destf){
  #writeLines("<substituteName>",destf)
  #write identifier>    </identifier>
 srcf<-write_next_token(srcf,destf)
 # writeLines("</substituteName>",destf)
  return(srcf)
}

parameterListf<-function(srcf,destf){
  writeLines("<parameterList>",destf)
  word2=get_word_from_line(2,srcf[1])
  if(word2!=")"){
    srcf<-typef(srcf,destf)
    srcf<-varNamef(srcf,destf)
    repeat{
      word2=get_word_from_line(2,srcf[1])
      if(word2!=","){
        break
      }
      else{
        srcf<-write_next_token(srcf,destf)
        srcf<-typef(srcf,destf)
        srcf<-varNamef(srcf,destf)
      }
  }
  }
  writeLines("</parameterList>",destf)
  return(srcf)
}

varDecf<-function(srcf,destf){
  writeLines("<varDec>",destf)
  #write <keyword> var </keyword>
  srcf<-write_next_token(srcf,destf)
  srcf<-typef(srcf,destf)
  srcf<-varNamef(srcf,destf)
  repeat{
    word2=get_word_from_line(2,srcf[1])
    if(word2!=","){
      break
    }
    else{
      #write <symbol> , </symbol>
      srcf<-write_next_token(srcf,destf)
      srcf<-varNamef(srcf,destf)
    }
  }
  #write <symbol> ; </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</varDec>",destf)
  return(srcf)
}

unaryOpf<-function(srcf,destf){
 # writeLines("<unaryOp>",destf)
  #write <symbol>  </symbol>
  srcf<-write_next_token(srcf,destf)
 # writeLines("</unaryOp>",destf)
  return(srcf)
}

opf<-function(srcf,destf){
 # writeLines("<op>",destf)
  #write <symbol>  </symbol>
  srcf<-write_next_token(srcf,destf)
 # writeLines("</op>",destf)
  return(srcf)
}

expressionListf<-function(srcf,destf){
  writeLines("<expressionList>",destf)
  word1<-get_word_from_line(1,srcf[1])
  word2<-get_word_from_line(2,srcf[1])
  if((word1=="<integerConstant>")||(word1=="<stringConstant>")||(word1=="<keyword>")||(word1=="<identifier>")||(word2=="(")||(word2=="-")||(word2=="~")){
   srcf<-expressionf(srcf,destf)
   repeat{
     word2=get_word_from_line(2,srcf[1])
     if(word2!=","){
       break
     }
     else{
       srcf<-write_next_token(srcf,destf)
       srcf<-expressionf(srcf,destf)
     }
   }
  }
  writeLines("</expressionList>",destf)
  return(srcf)
}

subroutineCallf<-function(srcf,destf){
 # writeLines("<subroutineCall>",destf)
  word1_current_line=get_word_from_line(1,srcf[1])
  word2_current_line=get_word_from_line(2,srcf[1])
  #next_words_line
  word2_next_line=get_word_from_line(2,srcf[2])
  #subroutineName(expressionList)
  if(word2_next_line=='('){
  srcf<-subroutineNamef(srcf,destf)
  #(
  srcf<-write_next_token(srcf,destf)
  ###########
  srcf<-expressionListf(srcf,destf)
  #)
  srcf<-write_next_token(srcf,destf)
  }
  #########################className | varName
  else{
    #className | varName
    src<-write_next_token(srcf,destf)
    #.
    srcf<-write_next_token(srcf,destf)
    #
    srcf<-subroutineNamef(srcf,destf)
    #(
    srcf<-write_next_token(srcf,destf)
    srcf<-expressionListf(srcf,destf)
    #)
    srcf<-write_next_token(srcf,destf)
  }
 # writeLines("</subroutineCall>",destf)
  return(srcf)
}

termf<-function(srcf,destf){
  writeLines("<term>",destf)
  word1_current_line=get_word_from_line(1,srcf[1])
  word2_current_line=get_word_from_line(2,srcf[1])
  #next_words_line
  word2_next_line=get_word_from_line(2,srcf[2])
  if((word1_current_line=="<integerConstant>")||(word1_current_line=="<stringConstant>")||(word1_current_line=="<keyword>")){
    srcf<-write_next_token(srcf,destf)
    writeLines("</term>",destf)
    return(srcf)
  }
  #(expression)
  if(word2_current_line=="("){
    #(
    srcf<-write_next_token(srcf,destf)
    #expresion
    srcf<-expressionf(srcf,destf)
    #)
    srcf<-write_next_token(srcf,destf)
    writeLines("</term>",destf)
    return (srcf)
  }
  #unaryOp term 
  else if((word2_current_line=="-")||(word2_current_line=="~")){
    srcf<-write_next_token(srcf,destf)
    #srcf<-unaryOpf(srcf,destf)
    srcf<-termf(srcf,destf)
    writeLines("</term>",destf)
    return (srcf)
  }
  #identifier- varName | varName[expression] | subrutineCall
  else{
    #varName[expression] 
    if(word2_next_line=="["){
      srcf<-varNamef(srcf,destf)
      #[
      srcf<-write_next_token(srcf,destf)
      srcf<-expressionf(srcf,destf)
      #]
      srcf<-write_next_token(srcf,destf)
    }
    #subrutineCall
    else if((word2_next_line=="(")||word2_next_line=="."){
      ##########
      srcf<-subroutineCallf(srcf,destf)
    }
    else{
      #varName
      srcf<-varNamef(srcf,destf)
    }
    writeLines("</term>",destf)
    return(srcf)
  }
}

expressionf<-function(srcf,destf){
  writeLines("<expression>",destf)
  srcf<-termf(srcf,destf)
  repeat{
    word2=get_word_from_line(2,srcf[1])
    if(check_element_in_list(word2,op_list)==FALSE){
      print(word2)
      break
    }
    else{
      #srcf<-opf(srcf,destf)
      srcf<-write_next_token(srcf,destf)
      srcf<-termf(srcf,destf)
    }
  }
  writeLines("</expression>",destf)
  return (srcf)
}


whileStatmentf<-function(srcf,destf){
  writeLines("<whileStatement>",destf)
  #write <keyword> while </keyword>
  srcf<-write_next_token(srcf,destf)
  #write <symbol> ( </symbol>
  srcf<-write_next_token(srcf,destf)
  srcf<-expressionf(srcf,destf)
  #write <symbol> ) </symbol>
  srcf<-write_next_token(srcf,destf)
  #write <symbol> { </symbol>
  srcf<-write_next_token(srcf,destf)
  srcf<-statmentsf(srcf,destf)
  #write <symbol> } </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</whileStatement>",destf)
  return (srcf)
}

#returnStatment: 'return' expression?';'
returnStatmentf<-function(srcf,destf){
  writeLines("<returnStatement>",destf)
  #write <keyword> return </keyword>
  srcf<-write_next_token(srcf,destf)
  #get first word from current line in srcf to check if we have expression
  #word2=the token type
  word1<-get_word_from_line(1,srcf[1])
  #get second word from current line in srcf to check if it is it one of the first termunal of expression
  #word2=the token value
  word2<-get_word_from_line(2,srcf[1])
  if((word1=="<integerConstant>")||(word1=="<stringConstant>")||(word1=="<keyword>")||(word1=="<identifier>")||(word2=="(")||(word2=="-")||(word2=="~")){
    #expression
    srcf<-expressionf(srcf,destf)
  }
  #write <symbol> ; </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</returnStatement>",destf)
  return (srcf)
}

#doStatment: do subroutineCall;
doStatmentf<-function(srcf,destf){
  writeLines("<doStatement>",destf)
  #write <keyword> do </keyword>
  srcf<-write_next_token(srcf,destf)
  #subroutineCall
  srcf<-subroutineCallf(srcf,destf)
  #write <symbol> ; </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</doStatement>",destf)
  return (srcf)
}

#ifStatment: 'if' '('expression')' '{'statments'}' ('else''{'statments'}')?
ifStatmentf<-function(srcf,destf){
  writeLines("<ifStatement>",destf)
  #write <keyword> if </keyword>
  srcf<-write_next_token(srcf,destf)
  #write <symbol> ( </symbol>
  srcf<-write_next_token(srcf,destf)
  #expression
  srcf<-expressionf(srcf,destf)
  #write <symbol> ) </symbol>
  srcf<-write_next_token(srcf,destf)
  #write <symbol> { </symbol>
  srcf<-write_next_token(srcf,destf)
  #statments
  srcf<-statmentsf(srcf,destf)
  #write <symbol> } </symbol>
  srcf<-write_next_token(srcf,destf)
  #get second word from current line in srcf to check if there is 'else {statments}'
  #word2=the token value
  word2=get_word_from_line(2,srcf[1])
  if(word2=="else"){
    #write <keyword> else </keyword>
    srcf<-write_next_token(srcf,destf)
    #write <symbol> { </symbol>
    srcf<-write_next_token(srcf,destf)
    #statments
    srcf<-statmentsf(srcf,destf)
    #write <symbol> } </symbol>
    srcf<-write_next_token(srcf,destf)
  }
  writeLines("</ifStatement>",destf)
  return (srcf)
}

#letStatment: 'let' varName ('['expression']')? '='expression';'
letStatmentf<-function(srcf,destf){
  writeLines("<letStatement>",destf)
  #write <keyword> let </keyword>
  srcf<-write_next_token(srcf,destf)
  #varName
  srcf<-varNamef(srcf,destf)
  #get second word from current line in srcf to check if there is '[expression]'
  #word2=the token value
  word2=get_word_from_line(2,srcf[1])
  #[expression]
  if(word2=='['){
    #write <symbol> [ </symbol>
    srcf<-write_next_token(srcf,destf)
    #expression
    srcf<-expressionf(srcf,destf)
    #write <symbol> ] </symbol>
    srcf<-write_next_token(srcf,destf)
  }
  #write <symbol> = </symbol>
  srcf<-write_next_token(srcf,destf)
  #expression
  srcf<-expressionf(srcf,destf)
  #write <symbol> ; </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</letStatement>",destf)
  return (srcf)
}

#statment: letStatment| ifStatment| whileStatmentf| doStatment| returnStatment
statmentf<-function(srcf,destf){
  #get second word from current line in srcf to get  the beginnig terminal of the statment type
  #word2=the token value
  word2=get_word_from_line(2,srcf[1])
  #calls to the method according to word2
  switch( 
    word2, 
    "let"=srcf<-letStatmentf(srcf,destf),
    "if"=srcf<-ifStatmentf(srcf,destf),
    "while"=srcf<-whileStatmentf(srcf,destf),
    "do"=srcf<-doStatmentf(srcf,destf),
    "return"=srcf<-returnStatmentf(srcf,destf))
  return (srcf)
}

#statments: statment*
statmentsf<-function(srcf,destf){
  writeLines("<statements>",destf)
  #statment*
  repeat{
    #get second word from current line in srcf to check if it one of  the beginnig terminal of statment
    #word2=the token value
    word2=get_word_from_line(2,srcf[1])
    #if word2 is not beginning terminals of statment (let /if/ do/ return/ while) ,break from loop
    if((word2!="let")&&(word2!="if")&&(word2!="while")&&(word2!="do")&&(word2!="return")){
      break
    }
    else{
      ##if word2 is  beginning terminals of statment (let /if/ do/ return/ while) , calls statmentf method
      srcf<-statmentf(srcf,destf)
    }
  }
  writeLines("</statements>",destf)
  return (srcf)
}

#subroutineBody: '{' varDec* statments'}'
subroutineBodyf<-function(srcf,destf){
  writeLines("<subroutineBody>",destf)
  #writes <symbol>  { </symbol>
  srcf<-write_next_token(srcf,destf)
  #varDec*
  repeat{
    #get second word from current line in srcf to check if it 'var' terminal- the beginnig of varDec
    #word2=the token value
    word2=get_word_from_line(2,srcf[1])
    #if word2 is not 'var' terminal, breaks from loop
    if(word2!="var"){
      break
    }
    #if word2 is 'var' terminal,calls varDecf method
    else{
      srcf<-varDecf(srcf,destf)
    }
  }
  #statments
  srcf<-statmentsf(srcf,destf)
  #writes <symbol>  } </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</subroutineBody>",destf)
  return(srcf)
}

#subroutineDecf: ('consructor'/'function'/'method') ('void'/type) sunroutineName'('parameterList')' subroutineBody
subroutineDecf<-function(srcf,destf){
  writeLines("<subroutineDec>",destf)
  #writes <keyword> consructor/function/method </keyword>
  srcf<-write_next_token(srcf,destf)
  #get second word from current line in srcf to check if it 'void' terminal or 'type' terminal
  #word2=the token value
  word2=get_word_from_line(2,srcf[1])
  #if word2='void'
  if(word2=="void"){
    #writes <keyword> void </keyword>
    srcf<-write_next_token(srcf,destf)
  }
  #if word2= type terminal, calls typef method
  else{
    srcf<-typef(srcf,destf)
  }
  #subroutineName
  srcf<-subroutineNamef(srcf,destf)
  #write <symbol> ( </symbol>
  srcf<-write_next_token(srcf,destf)
  #parameterList
  srcf<-parameterListf(srcf,destf)
  #write <symbol> ) </symbol>
  srcf<-write_next_token(srcf,destf)
  #subroutineBody
  srcf<-subroutineBodyf(srcf,destf)
  writeLines("</subroutineDec>",destf)
  return (srcf)
}


#class: 'class' className '{' classVarDec* subroutineDecf* '}'
classf<-function(srcf,destf){
  writeLines("<class>",destf)
  #writes <keyword> class </keyword>
  srcf<-write_next_token(srcf,destf)
  #write identifier>  </identifier>
  srcf<-classNamef(srcf,destf)
  #writes <symbol> { </symbol>
  srcf<-write_next_token(srcf,destf)
  #classVarDec*
  repeat{
    #get second word from current line in srcf to check if it one of the beginning terminals of classVarDec
    #word2=the token value
    word2=get_word_from_line(2,srcf[1])
    #if word2 is not beginning terminals of classVarDec (static /field) break from loop
    if((word2!="static")&&(word2!="field")){
      break
    }
    #if word2 is beginning terminals of classVarDec (static /field), calls classVarDec method
    else{
      srcf<-classVarDec(srcf,destf)
    }
  }
  #subroutineDec*
  repeat{
    #get second word from current line in srcf to check if it one of the beginning terminals of subroutineDec*
    #word2=the token value
    word2=get_word_from_line(2,srcf[1])
    #if word2 is not beginning terminals of subroutineDec (constructor /function/ method) break from loop
    if((word2!="constructor")&&(word2!="function")&&(word2!="method")){
      break
    }
    #if word2 is  beginning terminals of subroutineDec (constructor /function/ method),calls subroutineDecf method
    else{
      srcf<-subroutineDecf(srcf,destf)
    }
  }
  #writes <symbol> } </symbol>
  srcf<-write_next_token(srcf,destf)
  writeLines("</class>",destf)
  return(srcf)
}

#star parssing the current srcf file to destionation -destf
start_parssing_file<-function(srcf,destf){
  #every file beginning with class definition
  classf(srcf,destf)
}

tokenizer<-function(token,dest){
  setwd("C:\\R\\tar4\\project10")
  #dirs_list=list all  dirs in the folder
  dirs_list<-list.dirs(path = "C:\\R\\tar4\\project10", full.names = TRUE, recursive = FALSE)
  print(dirs_list)
  #loop over the dirs
  for (currentDir in dirs_list){
    setwd(currentDir)
    #concat th short dir name
    short_dir_name=paste(basename(currentDir))
    #gett list of all the 'jack' files that in the current dir
    jack_files_list<-list.files(path = currentDir,pattern="jack")
    print(jack_files_list)
    #go over the list of 'jack' files
    for (i in 1:length(jack_files_list)){
    #set the current working dir to read
    #src=open the current jack file to read the commands
    src=file(jack_files_list[i],'r')
    #extracts the file name without the extension
    shortfilename=sub("^([^.]*).*", "\\1", jack_files_list[i])
    #the destination file name is like the src name with "T.xml" extension
    destfileName=paste(shortfilename,"T.xml",sep="")
    #fc=list all lines(commands) in the current jack file
    fc <- readLines(src, -1)
    #the dir workink name to write is like the dir we read from, and it is in new folder that calls "xmlFIles"
    new_dir_path=paste("C:\\R\\tar4\\TxmlFiles\\",short_dir_name,sep="")
    #set the current working dir to write
    setwd(new_dir_path)
    #open the destination file to write
    dest<-file(destfileName,"w") 
    #Writes the outer token opening-<tokens>
    writeLines("<tokens>",dest)
    #loop over the lines of the current jack file
    for (i in 1:length(fc))
    {
       #sentence=current line
       sentence<-fc[i]
       #flag that says if the character '/' is divition or part of commend
       div_or_comment<-0
       #Remove leading  whitespace from sentence 
       triml<-trimws(sentence, "l")
       #gett first character in the line
       first_letter_line<-substr(triml, 1, 1)
        #Removes lines of comments and blank lines
        if((sentence!="")&&(sentence!="\t")&&(first_letter_line!='/')&&(first_letter_line!='*')){
        #Divides the line into characters
        characters_split <- strsplit(sentence, "")[[1]]
        #Initializes buffer string
        buffer_string<-character()
        #Initializes string constant buffer
        string_constant_buffer=character()
        #flag that says if we go over string constant
        in_string_constant=FALSE
        last_empty_char=FALSE
        #loop over the character split
       for (my_char in characters_split){
         #Ignoring spaces and check if we are noT in the middle of go over a string constant
         if((my_char!=" ")&&(my_char!="\t")&&(in_string_constant==FALSE)){
           #if 'my_char' is '"' ,puts in the flag True that says that it is  beginning string_costant
           if(my_char=='"'){
             in_string_constant=TRUE
           }
           #if 'my_char' is not a symbol , adds to the buffer_string
           else if(check_element_in_list(my_char,symbols_list)==FALSE){
             #buffer_string=buffer_string+my_char
             buffer_string<-paste (buffer_string,my_char, sep = "" )
           }
           #if 'my_char' is a symbol
           else if(check_element_in_list(my_char,symbols_list)==TRUE){
             #check if the Buffer string is noe empty
             if(length(buffer_string)>0){
               #gett first letter of the buffer string
               first_letter<-substring(buffer_string, 1, 1)
               #if there was symbol '/' that means divition
               if(div_or_comment==1){
                 #initialization div_or_comment flag
                 div_or_comment<-0
                 #writes '/' token
                 write_token("symbol",'/',dest)
               }
               #if first letter is digit - writes integerConstant token
               if(isDigit(first_letter)==TRUE){
                 write_token("integerConstant",buffer_string,dest)
               }
               #checks if buffer_string is in keywords list
               else if(check_element_in_list(buffer_string,keywords_list)==TRUE){
                 #writes keywords token
                 write_token("keyword",buffer_string,dest)
               }
               #else- writes "identifier" token
               else{
                 write_token("identifier",buffer_string,dest)
               }
               #Reset buffer string
               buffer_string<-character()
             }
             #remover commends that in line after code
             if((my_char!="/")&&(my_char!="*")){
                 write_token("symbol",my_char,dest)
              print(my_char)
             }
             #if my_char=="/" ,puts div_or_comment=1, and according the next character We'll know if it belongs to command or to divition token
             else if((my_char=="/")&&(div_or_comment==0)){
               div_or_comment<-1
             }
             #if my_char='/' or '*' and div_or_comment=1 that means the last character was '/' and it is a command now
             else if((((my_char=="/")||my_char=="*"))&&(div_or_comment==1)){
               #Reset div_or_comment flag
               div_or_comment<-0
               #goes to the next line in the file
               break
             }
           }
         }
         
         #when my_char=" " or my_char=="\t" or in_string_constant=TRUE
         else if((length(buffer_string)>0)&&(in_string_constant==FALSE)){
           #(buffer_string)
           first_letter<-substring(buffer_string, 1, 1)
           if(isDigit(first_letter)==TRUE){
             write_token("integerConstant",buffer_string,dest)
           }
           else if(check_element_in_list(buffer_string,keywords_list)==TRUE){
             write_token("keyword",buffer_string,dest)
           }
           else{
             write_token("identifier",buffer_string,dest)
           }
           buffer_string<-character()
         }
         #end_string_constant
         else if((in_string_constant==TRUE)&&(my_char=='"')){
           in_string_constant=FALSE
          # print(string_constant_buffer)
           write_token("stringConstant",string_constant_buffer,dest)
           string_constant_buffer=character()
         }
         
         else if(in_string_constant==TRUE){
           # print(my_char)
           if((my_char!=" ")&&(my_char!="\t")&&last_empty_char==TRUE){
             string_constant_buffer=paste(string_constant_buffer,my_char, sep = " " )
           }
           else{
             if((my_char==" ")&&(my_char=="\t"))
               last_empty_char=TRUE
             string_constant_buffer=paste(string_constant_buffer,my_char, sep = "" )
           }
         }
       }
     }
   }
  
   writeLines("</tokens>",dest)
   close(src)
   close(dest)
   
  }
}
}

if(FALSE){
 parsserf<-function(){
  setwd("C:\\R\\tar4\\project10")
  #dirs_list=list all  dirs in the folder
  dirs_list<-list.dirs(path ="C:\\R\\tar4\\project10", full.names = TRUE, recursive = FALSE)
  #loop over the dirs
  for (currentDir in dirs_list){
    short_dir_name=paste(basename(currentDir))
    #gett list of all the 'jack' files that in the current dir
    xml_files_list<-list.files(path = currentDir,pattern="xml")
    #go over the list of 'jack' files
    for (i in 1:length(xml_files_list)){
    #set the current working dir to read
    setwd(currentDir)
    #src=open the current jack file to read the commands
    src=file(xml_files_list[i],'r')
    #extracts the file name without the extension
    shortfilename=sub("^([^.]*).*", "\\1", xml_files_list[i])
    shortfilename=substr(shortfilename, 1, nchar(shortfilename)-1) 
    #the destination file name is like the src name with "T.xml" extension
    destfileName=paste(shortfilename,".xml",sep="")
    fc <- readLines(src, -1)
    #the dir workink name to write is like the dir we read from, and it is in new folder that calls "xmlFIles"
    #new_dir_path=paste("C:\\Users\\tamar\\Desktop\\tar4\\xmlFIles\\",short_dir_name,sep="")
    #set the current working dir to write
    #setwd(new_dir_path)
    #open the destination file to write
    dest<-file(destfileName,"w") 
   # dest<-file("wwwww") 
    print(shortfilename)
    #print(length(fc))
    start_parssing_file(fc[3:length(fc)-1],dest)
    #loop over the lines of the current xml file
    #for (i in 1:length(fc))
    #{
      #sentence=current line
     # sentence<-fc[i]
      
    #}
    close(src)
    close(dest)
    }
  }
######}
######parsserf()
 }
}
 
tokenizer()