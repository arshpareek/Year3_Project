/*** 
I verify that I am the sole author of the source code contained in this file,
except where explicitly stated to the contrary.

The following code is used to collect data related to the DFA based lexical analyser.

Author: Arshdeep Singh Pareek
Date: April 9, 2021
***/
%{ 
    #include <time.h>
%} 
  
/*** Rules defining the WHILE language and some regular expressions for experiments***/
%% 
(a*)*b  {printf("%s ((a*)*b)\n", yytext);}
(a|aa)* {printf("%s ((a|aa)*)\n", yytext);}
(a)?{10,11}a{10,11} {printf("%s ((a?){10, 10}(a){10, 10})\n", yytext);}
((a)+(a)+)+b {printf("%s ((a+a+)+b)\n", yytext);}
while|if|then|else|do|for|to|true|false|read|write|skip {printf("%s keyword\n", yytext);} 
[a-zA-Z]([a-zA-Z]|[0-9]|\_)* {printf("%s id\n", yytext);} 
\+|\-|\*|\%|\/|\=\=|\!\=|\>|\<|\<\=|\>\=|\:\=|\&\& {printf("%s op\n", yytext);}
0|([1-9]([0-9]*)) {printf("%s num\n", yytext);} 
; {printf("%s semi\n", yytext);} 
\"([a-zA-Z]|[0-9]|\_)*\" {printf("%s str\n", yytext);} 
[a-zA-Z]|[._><;=,:\\] {printf("%s sym\n", yytext);} 
\/\/(([a-zA-Z]|[._><;=,:\\])|\ |\t|[0-9])*(\n)? {printf("%s comments\n", yytext);} 
[{(})] {printf("%s par\n", yytext);} 
[\t\n\ ]+ {printf("%s whitespace\n", yytext);} 
.     {printf("%s not part of WHILE language\n", yytext);} 
%% 
  

int yywrap(){} 
int main(){ 
  
// Import source code file.
extern FILE* yyin;
yyin=fopen("factorial.while","r");

// Measure time taken to lex the program.
clock_t start = clock();
yylex(); 
clock_t end = clock();
int diff = (double) ((end-start)/(CLOCKS_PER_SEC/1000000));

// Print the measurement to the console.
printf("\nElapsed time in " 
      "microseconds - %d\n", diff); 
  
return 0; 
} 