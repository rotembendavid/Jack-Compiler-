#Targil 5
#Tamar Appelboim 208810507
#Rotem Ben David 315658773

#keywords list
keywords_list=list('class','constructor','function','method','field','static','var','int','char','boolean','void','true','false','null','this','let','do','if','else','while','return')
#symbols list
symbols_list=list('{','}','(',')','[',']','.',',',';','+','-','*','/','&','|','<','>','=','~')
#op list
op_list=list('+','-','*','/','&','|','<','>','=','&lt;','&gt;','&amp;')

while_counter<-0
if_counter<-0
local_counter<-0
field_counter<-0
argument_counter<-0
current_class_name<-NULL

#record of a line in the table
Record<-setClass("Record",slots = list(name="character",type="character",kind="character",num="numeric"))

#returns ascii code
asc <- function(x) { strtoi(charToRaw(x),16L) }

write_push<-function(type,index,dest){
  line=paste("push",type,index,sep=" ")
  writeLines(line,dest)
}

write_pop<-function(type,index,dest){
  line=paste("pop",type,index,sep=" ")
  writeLines(line,dest)
}

write_call<-function(name,num_args,dest){
  line=paste("call",name,num_args,sep=" ")
  writeLines(line,dest)
}

write_op<-function(op,dest){
  writeLines(op,dest)
}

write_label<-function(label,dest){
  line=paste("label",label,sep=" ")
  writeLines(line,dest)
}

write_ifgoto<-function(label,dest){
  line=paste("if-goto",label,sep=" ")
  writeLines(line,dest)
}

write_goto<-function(label,dest){
  line=paste("goto",label,sep=" ")
  writeLines(line,dest)
}

write_function<-function(name,num_locals,dest){
  line=paste("function",name,num_locals,sep=" ")
  writeLines(line,dest)
}

write_return<-function(dest){
  writeLines("return",dest)
}

kindOf<-function(name,class_table,subroutine_table){
  if((length(subroutine_table)>0)){
  for (current in subroutine_table[[1]]){
    if((current[1])==name){
      return(current[3])
    }
  }
  }
  if(length(class_table)>0){
  for (current in class_table[[1]]){
    if((current[1])==name){
      return(current[3])
    }
  }
  }
}

typeof<-function(name,class_table,subroutine_table){
  for (current in subroutine_table[[1]]){
    if((current[1])==name){
      return (current[2])
    }
  }
  for (current in class_table[[1]]){
    if((current[1])==name){
      return (current[2])
    }
  }
}


indexOf<-function(name,class_table,subroutine_table){
  for (current in subroutine_table[[1]]){
    if(current[1]==name){
      return(current[4])
    }
  }
  for (current in class_table[[1]]){
    if(current[1]==name){
      return(current[4])
    }
  }
}

get_object_of_static_function<-function(class_name,subroutine_table){
  if(length(subroutine_table>0)){
  for (current in subroutine_table[[1]]){
    if((current[2])==class_name){
      return (current[1])
    }
  }
  }
}

push_varName<-function(name,class_table,subroutine_table,dest){
  type=kindOf(name,class_table,subroutine_table)
  index=indexOf(name,class_table,subroutine_table)
  if(type=='var'){
    write_push('local',index,dest)
  }
  else if(type=='argument'){
      write_push('argument',index,dest)
  }
  else if(type=='field'){
    write_push('this',index,dest)
  }
  else{
    write_push('static',index,dest)
  }
}

pop_to_varName<-function(name,class_table,subroutine_table,dest){
  type=kindOf(name,class_table,subroutine_table)
  index=indexOf(name,class_table,subroutine_table)
  if(type=='var'){
    write_pop('local',index,dest)
  }
  else if(type=='argument'){
    write_pop('argument',index,dest)
  }
  else if(type=='field'){
    write_pop('this',index,dest)
  }
  else{
    write_pop('static',index,dest)
  }
}

create_class_scope_symbol_table<-function(srcf){
  class_scope_symbol_table<-list()
  static_counter<-0
  list_counter<-1
  repeat{
    word2<-get_word_from_line(2,srcf[1])
    if((word2!='static')&&(word2!='field')){
      break
    }
    #field
    else{
      #skip static/field word
      srcf<-get_src_without_first_line(srcf)
      if (word2=='static'){
        kind_temp='static'
        type_temp=get_word_from_line(2,srcf[1])
        #skip type
        srcf<-get_src_without_first_line(srcf)
        repeat{
          #get second word from current line in srcf to check if it ',' terminal 
          name_temp=get_word_from_line(2,srcf[1])
          #skip name
          srcf<-get_src_without_first_line(srcf)
          new_record=list(name=name_temp,type=type_temp,kind=kind_temp,num=static_counter)
          class(new_record) <- "Record"
          static_counter=static_counter+1
          class_scope_symbol_table[[list_counter]]<-new_record
          list_counter=list_counter+1
          word2<-get_word_from_line(2,srcf[1])
          #if word2 is not ',', breaks from the loop
            if(word2!=","){
              #;
              srcf<-get_src_without_first_line(srcf)
              break
              }
              else{
              #skip ,
              srcf<-get_src_without_first_line(srcf)
              }
       }
      }
      else if (word2=='field'){
        kind_temp='field'
        #skip field on the top lines
        type_temp=get_word_from_line(2,srcf[1])
        #skip type
        srcf<-get_src_without_first_line(srcf)
        repeat{
          name_temp=get_word_from_line(2,srcf[1])
          #skip name
          srcf<-get_src_without_first_line(srcf)
          new_record<-list(name=name_temp,type=type_temp,kind=kind_temp,num=field_counter)
          class(new_record) <- "Record"
          assign("field_counter",field_counter+1, envir = .GlobalEnv)
          class_scope_symbol_table[[list_counter]]=new_record
          list_counter=list_counter+1
          word2<-get_word_from_line(2,srcf[1])
          #if word2 is not ',', breaks from the loop
          if(word2!=","){
            #skip ;
            srcf<-get_src_without_first_line(srcf)
            break
          }
          else{
            #skip ,
            srcf<-get_src_without_first_line(srcf)
          }
        }
     }
    }
  }
  listT<-list(class_scope_symbol_table,srcf)
  return(listT)
  }
  
create_method_scope_symbol_table<-function(type_subroutine,srcf){
  method_scope_table<-list()
  table_counter<-1
  if(type_subroutine=='method'){
    new_record<-list(name='this',type=current_class_name,kind="argument",num=argument_counter)
    class(new_record) <- "Record"
    method_scope_table[[table_counter]]<-new_record
    assign("argument_counter",argument_counter+1, envir = .GlobalEnv)
    table_counter=table_counter+1
  }
  #skipp (
  srcf<-get_src_without_first_line(srcf)
  word2<-get_word_from_line(2,srcf[1])
  #parameter list
  if(word2!=')'){
    repeat{
      type_temp<-get_word_from_line(2,srcf[1])
      #skips arg type
      srcf<-get_src_without_first_line(srcf)
      name_temp<-get_word_from_line(2,srcf[1])
      #skips arg name
      srcf<-get_src_without_first_line(srcf)
      new_record<-list(name=name_temp,type=type_temp,kind="argument",num=argument_counter)
      class(new_record) <- "Record"
      method_scope_table[[table_counter]]<-new_record
      assign("argument_counter",argument_counter+1, envir = .GlobalEnv)
      table_counter=table_counter+1
      word2=get_word_from_line(2,srcf[1])
      #if word2 is not ',', breaks from the loop
      if(word2!=","){
        break
      }
      else{
        #,
        srcf<-get_src_without_first_line(srcf)
      }
    }
  }
  #)
  srcf<-get_src_without_first_line(srcf)
  #{
  srcf<-get_src_without_first_line(srcf)
  #locals
  repeat{
    word2<-get_word_from_line(2,srcf)
    #if the next line is not var declaration
    if(word2!='var'){
      break
    }
    else{
      srcf<-get_src_without_first_line(srcf)
      type_temp<-get_word_from_line(2,srcf[1])
      #skip type
      srcf<-get_src_without_first_line(srcf)
      repeat{
        name_temp=get_word_from_line(2,srcf[1])
        #skips name
        srcf<-get_src_without_first_line(srcf)
        new_record<-list(name=name_temp,type=type_temp,kind="var",num=local_counter)
        class(new_record) <- "Record"
        method_scope_table[[table_counter]]<-new_record
        table_counter=table_counter+1
        assign("local_counter",local_counter+1, envir = .GlobalEnv)
        current_token<-get_word_from_line(2,srcf[1])
        if(current_token!=','){
          #skips ;
          srcf<-get_src_without_first_line(srcf)
          break
        }
        else{
          #skips ,
          srcf<-get_src_without_first_line(srcf)
        }
      }
    }
  }
  listT<-list(method_scope_table,srcf)
  return (listT)
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


#the function cuts the first line from srcf and return it
get_src_without_first_line<-function(srcf){
  return (srcf[2:length(srcf)])
}

 
#the function get a line from srcf and number ,and returns the word in line according to the number  
get_word_from_line<-function(word_num,line){
  #get line string with out left spaces
  line=trimws(line, "l")
  #split line string to list of world
  words<-unlist(strsplit(line," "))
  #return the wotd in place word_num- the number that the function got
  return(words[word_num])
}

#expressionList: (expression (,'expression)*)?
expressionListf<-function(class_table,symbol_table,srcf,dest){
  exp_counter<-0
  word1<-get_word_from_line(1,srcf[1])
  word2<-get_word_from_line(2,srcf[1])
  # if there is values that match the expression- it is not empty
  if((word1=="<integerConstant>")||(word1=="<stringConstant>")||(word1=="<keyword>")||(word1=="<identifier>")||(word2=="(")||(word2=="-")||(word2=="~")){
    #expression
    srcf<-expressionf(class_table,symbol_table,srcf,dest)
    exp_counter=exp_counter+1
    #(','expression)*
   repeat{
     #get second word from current line in srcf to check if it ',' terminal
     word2=get_word_from_line(2,srcf[1])
     #if word2 is not ',' terminal, breaks from loop
     if(word2!=","){
       break
     }
     else{
       #if word2 is ',' terminal
       #w, 
       srcf<-get_src_without_first_line(srcf)
       #expression
       srcf<-expressionf(class_table,symbol_table,srcf,dest)
       exp_counter=exp_counter+1
     }
   }
  }
  listT<-list()
  listT[[1]]<-exp_counter
  listT[[2]]<-srcf
  return(listT)
}


#term: integerConstant| stringConstant | keywordConstant |varName | varName'['expression']' | subrutineCall | '('expression')' | unaryOp term
termf<-function(class_table,symbol_table,srcf,dest){
  word1_current_line=get_word_from_line(1,srcf[1])
  if(word1_current_line=="<stringConstant>"){
    
    line<-srcf[1]
    line<-trimws(line, "l")
    words<-unlist(strsplit(line," "))
    words_list<-words[3:length(words)-1]
    strinfConst<-words_list[1]
    if(length(words_list)>1){
      for (i in 2:length(words_list)){
        strinfConst<-paste(strinfConst,words_list[i],sep=" ")
      }
    }
    len_string<-str_length(strinfConst)
  }
  word2_current_line=get_word_from_line(2,srcf[1])
  word2_next_line=get_word_from_line(2,srcf[2])
  if(word1_current_line=="<integerConstant>"){
   write_push("constant",word2_current_line,dest)
    srcf<-get_src_without_first_line(srcf)
  }
  else if(word1_current_line=="<stringConstant>"){
    string_characters=strsplit(strinfConst, "")[[1]]
    #push  string constant length
    write_push('constant',len_string,dest)
    write_call('String.new',1,dest)
    for (character in string_characters){
      ascii_letter_code<-asc(character)
      write_push('constant',ascii_letter_code,dest)
      write_call('String.appendChar',2,dest)
    }
    srcf<-get_src_without_first_line(srcf)
  }
  else if(word1_current_line=="<keyword>"){
    #true,false,null,this
    #true
    if(word2_current_line=='true'){
      write_push('constant',0,dest)
      write_op('not',dest)
    }
    else if((word2_current_line=='false')||(word2_current_line=='null')){
      write_push('constant',0,dest)
    }
    else if(word2_current_line=='this'){
      write_push('pointer',0,dest)
    }
    srcf<-get_src_without_first_line(srcf)
  }
    #cases: word2_current_line=identifier- varName | varName[expression] | subrutineCall
  else if(word1_current_line=="<identifier>"){
    srcf<-get_src_without_first_line(srcf)
    #varName[expression] 
    if(word2_next_line=="["){
      #[ 
      srcf<-get_src_without_first_line(srcf)
      #expression
      srcf<-expressionf(class_table,symbol_table,srcf,dest)
      #varName
      push_varName(word2_current_line,class_table,symbol_table,dest)
      write_op('add',dest)
      write_pop("pointer",1,dest)
      write_push("that",0,dest)
      #] 
      srcf<-get_src_without_first_line(srcf)
    }
    #case: subrutineCall
   else if(word2_next_line=="("){
      #subroutineCall
      write_push("pointer",0,dest)
      #( 
      srcf<-get_src_without_first_line(srcf)
      listT<-expressionListf(class_table,symbol_table,srcf,dest)
      expression_counter<-listT[1][[1]]
      srcf<-listT[2]
      srcf<-srcf[[1]]
      sub_name=paste(current_class_name,".",word1_current_line,sep="")
      write_call(sub_name,expression_counter,dest)
      #)
      srcf<-get_src_without_first_line(srcf)
    }
   else if(word2_next_line=="."){
      #skip .
      srcf<-get_src_without_first_line(srcf)
      sub_name<-get_word_from_line(2,srcf[1])
      #sub_name
      srcf<-get_src_without_first_line(srcf)
      kind_var=kindOf(word2_current_line,class_table,symbol_table)
      #if kind_var!=null
      if(length(kind_var)>0){
        push_varName(word2_current_line,class_table,symbol_table,dest)
        type<-typeof(word2_current_line,class_table,symbol_table)
        #(
        srcf<-get_src_without_first_line(srcf)
        listT<-expressionListf(class_table,symbol_table,srcf,dest)
        expression_counter<-listT[1][[1]]
        srcf<-listT[2]
        srcf<-srcf[[1]]
        #)
        srcf<-get_src_without_first_line(srcf)
        sub_call_name<-paste(type,".",sub_name,sep="")
        write_call(sub_call_name,expression_counter+1,dest)
       }
        #clASS_NAME
     else{
        #(
        srcf<-get_src_without_first_line(srcf)
        listT<-expressionListf(class_table,symbol_table,srcf,dest)
        expression_counter<-listT[1][[1]]
        srcf<-listT[2]
        srcf<-srcf[[1]]
        #)
        srcf<-get_src_without_first_line(srcf)
        name_call_sub<-paste(word2_current_line,".",sub_name,sep = "")
        write_call(name_call_sub,expression_counter,dest)
        }
    }
      else{
        #case: varName
        push_varName(word2_current_line,class_table,symbol_table,dest)
      }
 }
  #case: (expression)
   else if(word2_current_line=="("){
    #( 
    srcf<-get_src_without_first_line(srcf)
    #expresion
    srcf<-expressionf(class_table,symbol_table,srcf,dest)
    #) 
    srcf<-get_src_without_first_line(srcf)
  }
    
  # case: unaryOp term 
  else if((word2_current_line=="-")||(word2_current_line=="~")){
     srcf<-get_src_without_first_line(srcf)
     srcf<-termf(class_table,symbol_table,srcf,dest)
    if(word2_current_line=="-"){
      write_op('neg',dest)
    }
     else{
       write_op('not',dest)
     }
  }
  return(srcf)
}


#expression: term (op term)*
expressionf<-function(class_table,symbol_table,srcf,dest){
  #term
  srcf<-termf(class_table,symbol_table,srcf,dest)
  #term (op term)*
  repeat{
    word2=get_word_from_line(2,srcf[1])
    if(check_element_in_list(word2,op_list)==FALSE){
      break
    }
    else{
      op<-word2
      srcf<-get_src_without_first_line(srcf)
      #term
      srcf<-termf(class_table,symbol_table,srcf,dest)
      if(op=='+'){
        write_op('add',dest)
      }
      else if(op=='-'){
        write_op('sub',dest)
      }
      else if(op=='*'){
        write_call("Math.multiply",2,dest)
      }
      else if(op=='/'){
        write_call("Math.divide",2,dest)
      }
      else if(op=='&amp;'){
        write_op('and',dest)
      }
      else if(op=='|'){
        write_op('or',dest)
      }
      else if(op=='&lt;'){
        write_op('lt',dest)
      }
      else if(op=='&gt;'){
        write_op('gt',dest)
      }
      else if(op=='='){
        write_op('eq',dest)
      }
    }
  }
  return(srcf)
}

#whileStatment: 'while' '('expression')' '{'statments'}'
whileStatmentf<-function(class_table,symbol_table,srcf,dest){
  while_current_counter<-while_counter
  assign("while_counter",while_counter+1, envir = .GlobalEnv)
  #while
  srcf<-get_src_without_first_line(srcf)
  #( 
  srcf<-get_src_without_first_line(srcf)
  #expression
  write_label(paste("WHILE",while_current_counter,sep=""),dest)
  srcf<-expressionf(class_table,symbol_table,srcf,dest)
  #) 
  srcf<-get_src_without_first_line(srcf)
  #{ 
  srcf<-get_src_without_first_line(srcf)
  write_op('not',dest)
  write_ifgoto(paste("WHILE_END",while_current_counter,sep=""),dest)
  #statments
  srcf<-statmentsf(class_table,symbol_table,srcf,dest)
  write_goto(paste("WHILE",while_current_counter,sep=""),dest)
  write_label(paste("WHILE_END",while_current_counter,sep=""),dest)
  #}
  srcf<-get_src_without_first_line(srcf)
  return (srcf)
}

#returnStatment: 'return' expression?';'
returnStatmentf<-function(class_table,symbol_table,srcf,dest){
  #return
  srcf<-get_src_without_first_line(srcf)
  word2<-get_word_from_line(2,srcf[1])
  if(word2==';'){
    write_push('constant',0,dest)
    write_return(dest)
    #;
    srcf<-get_src_without_first_line(srcf)
  }
  else{
   srcf<-expressionf(class_table,symbol_table,srcf,dest)
   write_return(dest)
   #;
   srcf<-get_src_without_first_line(srcf)
  }
  return (srcf)
}
 

#doStatment: do subroutineCall;
doStatmentf<-function(class_table,symbol_table,srcf,dest){
  #do 
  flag<-FALSE
  srcf<-get_src_without_first_line(srcf)
  #subroutineName|className|varName
  word2=get_word_from_line(2,srcf[1])
  word2_next_line<-get_word_from_line(2,srcf[2])
  #subroutineName(expression)
  if(word2_next_line=="("){
    subroutineName=word2
    #subName
    srcf<-get_src_without_first_line(srcf)
    #(
    srcf<-get_src_without_first_line(srcf)
    write_push('pointer',0,dest)
    listT<-expressionListf(class_table,symbol_table,srcf,dest)
    expression_counter<-listT[1][[1]]
    srcf<-listT[2]
    srcf<-srcf[[1]]
    sub_call_name<-paste(current_class_name,".",subroutineName,sep="")
    write_call(sub_call_name,expression_counter+1,dest)
    write_pop('temp',0,dest)
    #)
    srcf<-get_src_without_first_line(srcf)
  }
  #word2_next_line=="."
  else{
    #className/objectName
    srcf<-get_src_without_first_line(srcf)
    #.
    srcf<-get_src_without_first_line(srcf)
    subroutineName<-get_word_from_line(2,srcf[1])
    #subroutineName
    srcf<-get_src_without_first_line(srcf)
    #(
    srcf<-get_src_without_first_line(srcf)
    kind<-kindOf(word2,class_table,symbol_table)
    if(length(kind)>0){
      push_varName(word2,class_table,symbol_table,dest)
      type<-typeof(word2,class_table,symbol_table)
      listT<-expressionListf(class_table,symbol_table,srcf,dest)
      expression_counter<-listT[1][[1]]
      srcf<-listT[2]
      srcf<-srcf[[1]]
      #)
      srcf<-get_src_without_first_line(srcf)
      sub_call_name<-paste(type,".",subroutineName,sep="")
      write_call(sub_call_name,expression_counter+1,dest)
      write_pop('temp',0,dest)
      }
    #currentClass | otherClass
    else{
        listT<-expressionListf(class_table,symbol_table,srcf,dest)
        expression_counter<-listT[1][[1]]
        srcf<-listT[2]
        srcf<-srcf[[1]]
        srcf<-get_src_without_first_line(srcf)
        sub_call_name<-paste(word2,".",subroutineName,sep="")
        if(flag==TRUE){
          expression_counter<-expression_counter+1
        }
        write_call(sub_call_name,expression_counter,dest)
        write_pop('temp',0,dest)
      }
  }
  srcf<-get_src_without_first_line(srcf)
  return (srcf)
}
  


#ifStatment: 'if' '('expression')' '{'statments'}' ('else''{'statments'}')?
ifStatmentf<-function(class_table,symbol_table,srcf,dest){
  #if
  if_current_counter<-if_counter
  assign("if_counter",if_counter+1, envir = .GlobalEnv)
  srcf<-get_src_without_first_line(srcf)
  #(
  srcf<-get_src_without_first_line(srcf)
  #expression
  srcf<-expressionf(class_table,symbol_table,srcf,dest)
  write_ifgoto(paste("IF_TRUE",if_current_counter,sep=""),dest)
  write_goto(paste("IF_FALSE",if_current_counter,sep=""),dest)
  write_label(paste("IF_TRUE",if_current_counter,sep=""),dest)
  #) 
  srcf<-get_src_without_first_line(srcf)
  # { 
  srcf<-get_src_without_first_line(srcf)
  #statments
  srcf<-statmentsf(class_table,symbol_table,srcf,dest)
  #} 
  srcf<-get_src_without_first_line(srcf)
  word2=get_word_from_line(2,srcf[1])
  if(word2=="else"){
    #else
    srcf<-get_src_without_first_line(srcf)
    #{ 
    srcf<-get_src_without_first_line(srcf)
    write_goto(paste("IF_END",if_current_counter,sep=""),dest)
    write_label(paste("IF_FALSE",if_current_counter,sep=""),dest)
    #statments
    srcf<-statmentsf(class_table,symbol_table,srcf,dest)
    #} 
    srcf<-get_src_without_first_line(srcf)
    write_label(paste("IF_END",if_current_counter,sep=""),dest)
  }
  else{
    write_label(paste("IF_FALSE",if_current_counter,sep=""),dest)
  }
  return (srcf)
}

#letStatment: 'let' varName ('['expression']')? '='expression';'
letStatmentf<-function(class_table,symbol_table,srcf,dest){
  flag=FALSE
  #let
  srcf<-get_src_without_first_line(srcf)
  #varName
  varName<-get_word_from_line(2,srcf[1])
  #get second word from current line in srcf to check if there is '[expression]'
  #word2=the token value
  #skipp varName
  srcf<-get_src_without_first_line(srcf)
  word2=get_word_from_line(2,srcf[1])
  #[expression]
  if(word2=='['){
    flag=TRUE
    # [ 
    srcf<-get_src_without_first_line(srcf)
    #expression
    srcf<-expressionf(class_table,symbol_table,srcf,dest)
    push_varName(varName,class_table,symbol_table,dest)
    write_op('add',dest)
    #] 
    srcf<-get_src_without_first_line(srcf)
  }
  #= 
  srcf<-get_src_without_first_line(srcf)
  #expression
  srcf<-expressionf(class_table,symbol_table,srcf,dest)
  #; 
  srcf<-get_src_without_first_line(srcf)
  if(flag==TRUE){
    write_pop("temp",0,dest)
    write_pop("pointer",1,dest)
    write_push("temp",0,dest)
    write_pop("that",0,dest)
  }
  else{
    pop_to_varName(varName,class_table,symbol_table,dest)
  }
  return (srcf)
}


#statment: letStatment| ifStatment| whileStatmentf| doStatment| returnStatment
statmentf<-function(class_table,symbol_table,srcf,dest){
  #get second word from current line in srcf to get  the beginnig terminal of the statment type
  #word2=the token value
  word2=get_word_from_line(2,srcf[1])
  #calls to the method according to word2
  switch( 
    word2, 
    "let"=srcf<-letStatmentf(class_table,symbol_table,srcf,dest),
    "if"=srcf<-ifStatmentf(class_table,symbol_table,srcf,dest),
    "while"=srcf<-whileStatmentf(class_table,symbol_table,srcf,dest),
    "do"=srcf<-doStatmentf(class_table,symbol_table,srcf,dest),
    "return"=srcf<-returnStatmentf(class_table,symbol_table,srcf,dest))
  return (srcf)
}

#statments: statment*
statmentsf<-function(class_table,method_table,srcf,dest){
  repeat{
    word2=get_word_from_line(2,srcf[1])
    if((word2!="let")&&(word2!="if")&&(word2!="while")&&(word2!="do")&&(word2!="return")){
      break
    }
    else{
      #if word2 is  beginning terminals of statment (let /if/ do/ return/ while) , calls statmentf method
       srcf<-statmentf(class_table,method_table,srcf,dest)
    }
  }
  return (srcf)
}

#subroutineBody: '{' varDec* statments'}'
subroutineBodyf<-function(class_table,method_table,srcf,dest){
  #statments
  srcf<-statmentsf(class_table,method_table,srcf,dest)
  srcf<-get_src_without_first_line(srcf)
  return(srcf)
}

#subroutineDecf: ('consructor'/'function'/'method') ('void'/type) sunroutineName'('parameterList')' subroutineBody
subroutineDecf<-function(class_symbol,srcf,dest,field_count){
  type_subroutine<-get_word_from_line(2,srcf[1])
  #skipp consructor'/'function'/'method'
  srcf<-get_src_without_first_line(srcf)
  #skipp 'void'/type
  srcf<-get_src_without_first_line(srcf)
  subroutine_name<-get_word_from_line(2,srcf[1])
  #skipp subroutine_name
  srcf<-get_src_without_first_line(srcf)
  listT<-create_method_scope_symbol_table(type_subroutine,srcf)
  method_symbol_table<-listT[1]
  srcf<-listT[2]
  srcf<-list(srcf)
  srcf<-srcf[[1]][[1]]
  if(type_subroutine=='method'){
    line=paste(current_class_name,".",subroutine_name,sep="")
    write_function(line,local_counter,dest)
    write_push("argument",0,dest)
    write_pop("pointer",0,dest)
  }
  else if(type_subroutine=='constructor'){
    line=paste(current_class_name,".","new",sep="")
    write_function(line,0,dest)
    write_push("constant",field_count,dest)
    write_call("Memory.alloc",1,dest)
    write_pop("pointer",0,dest)
  }
  else{
    line=paste(current_class_name,".",subroutine_name,sep="")
    write_function(line,local_counter,dest)
  }
  srcf<-subroutineBodyf(class_symbol,method_symbol_table,srcf,dest)
  return (srcf)
}
  
    
 
#class: 'class' className '{' classVarDec* subroutineDecf* '}'
classf<-function(srcf,dest){
  assign("if_counter",0, envir = .GlobalEnv)
  assign("while_counter",0, envir = .GlobalEnv)
  assign("local_counter",0, envir = .GlobalEnv)
  assign("field_counter",0, envir = .GlobalEnv)
  #skipps 'class'
  srcf<-get_src_without_first_line(srcf)
  class_name<-get_word_from_line(2,srcf[1])
  #current_class_name
  assign("current_class_name",class_name, envir = .GlobalEnv)
  #skipps className
  srcf<-get_src_without_first_line(srcf)
  #skipps {
  srcf<-get_src_without_first_line(srcf)
  #classVarDec*
  word2<-get_word_from_line(2,srcf)
  if(word2=="static" || word2=="field"){
  listT<-create_class_scope_symbol_table(srcf)
  class_symbol_table<-listT[1]
  srcf<-listT[2]
  srcf<-srcf[[1]]
  }
  else{
    class_symbol_table<-list()
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
    #if word2 is  beginning terminals of subroutineDec (constructor /function/ method)
    else{
      assign("argument_counter",0, envir = .GlobalEnv)
      assign("local_counter",0, envir = .GlobalEnv)
      assign("if_counter",0, envir = .GlobalEnv)
      assign("while_counter",0, envir = .GlobalEnv)
      srcf<-subroutineDecf(class_symbol_table,srcf,dest,field_counter)
    }
  }
}



tokenizer<-function(){
#dirs_list=list all  dirs in the folder
dirs_list<-list.dirs(path = "C:\\Users\\tamar\\Desktop\\tar5\\project 11", full.names = TRUE, recursive = FALSE)
#loop over the dirs
for (currentDir in dirs_list){
  #concat th short dir name
  short_dir_name=paste(basename(currentDir))
  #gett list of all the 'jack' files that in the current dir
  jack_files_list<-list.files(path = currentDir,pattern="jack")
  #go over the list of 'jack' files
  for (i in 1:length(jack_files_list)){
  #set the current working dir to read
   setwd(currentDir)
  #src=open the current jack file to read the commands
  src=file(jack_files_list[i],'r')
  #extracts the file name without the extension
  shortfilename=sub("^([^.]*).*", "\\1", jack_files_list[i])
  #the destination file name is like the src name with "T.xml" extension
  destfileName=paste(shortfilename,"T.xml",sep="")
  #fc=list all lines(commands) in the current jack file
   fc <- readLines(src, -1)
   #the dir workink name to write is like the dir we read from, and it is in new folder that calls "xmlFIles"
   new_dir_path=paste("C:\\Users\\tamar\\Desktop\\tar5\\xmlFIles\\",short_dir_name,sep="")
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
       #split the line into characters list
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
             #check if the Buffer string is not empty
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
             else{
               write_token("symbol",my_char,dest)
             }
           }
         }
         
         #when my_char=" " or my_char=="\t" or in_string_constant=TRUE
         else if((length(buffer_string)>0)&&(in_string_constant==FALSE)){
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
           write_token("stringConstant",string_constant_buffer,dest)
           string_constant_buffer=character()
         }
         
         else if(in_string_constant==TRUE){
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

parsserf<-function(){
  #dirs_list=list all  dirs in the folder
  dirs_list<-list.dirs(path ="C:\\Users\\tamar\\Desktop\\tar5\\xmlFiles", full.names = TRUE, recursive = FALSE)
  #loop over the dirs
  for (currentDir in dirs_list){
    short_dir_name=paste(basename(currentDir))
    #gett list of all the 'xml' files that in the current dir
    xml_files_list<-list.files(path = currentDir,pattern="xml")
    #go over the list of 'xml' files
    for (i in 1:length(xml_files_list)){
    #set the current working dir to read
    setwd(currentDir)
    #src=open the current xml file to read the lines
    src=file(xml_files_list[i],'r')
    #extracts the file name without the extension
    shortfilename=sub("^([^.]*).*", "\\1", xml_files_list[i])
    #sub the letter T from  shortfilename
    shortfilename=substr(shortfilename, 1, nchar(shortfilename)-1) 
    #the destination file name is like the src name with "xml" extension withoue 'T'
    destfileName=paste(shortfilename,".vm",sep="")
    #fc=list of lines src file
    fc <- readLines(src, -1)
    setwd(paste('C:\\Users\\tamar\\Desktop\\tar5\\vmfiles\\',short_dir_name,sep=""))
    #open the destination file to write
    dest<-file(destfileName,"w") 
    #start parssing , send src without <tokens>
    srcf<-fc[3:length(fc)-1]
    classf(srcf,dest)
    #close files
    close(src)
    close(dest)
    }
  }
}

tokenizer()
parsserf()