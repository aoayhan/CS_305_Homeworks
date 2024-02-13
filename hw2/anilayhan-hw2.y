%{
#include <stdio.h>
int yylex();
void yyerror(const char *s){
    return;}
%}

%token tMAIL tENDMAIL tSCHEDULE tENDSCHEDULE tSEND tSET tTO tFROM tAT tCOMMA tCOLON tLPR tRPR tLBR tRBR tIDENT tSTRING tADDRESS tDATE tTIME
%start program
%%

program:      
                | program component 
;

component: mail_block 
                | set_statement 
;
    
mail_block: tMAIL tFROM tADDRESS tCOLON statement_list tENDMAIL
                | tMAIL tFROM tADDRESS tCOLON tENDMAIL 
;

statement_list: statement
                | statement statement_list 
;

statement: set_statement 
                | send_statement 
                | schedule_statement 
;

set_statement: tSET tIDENT tLPR tSTRING tRPR 
;

send_statement: tSEND tLBR tSTRING tRBR tTO recipient_list 
| tSEND tLBR tIDENT tRBR tTO recipient_list 
;



recipient_list: tLBR recipient_list_elements tRBR 
;

recipient_list_elements: recipient 
                | recipient tCOMMA recipient_list_elements 
;

recipient: tLPR tADDRESS tRPR 
                | tLPR tIDENT tCOMMA tADDRESS tRPR 
                | tLPR tSTRING tCOMMA tADDRESS tRPR 
;

schedule_statement: tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON send_list tENDSCHEDULE 
;

send_list: send_statement 
                | send_statement send_list
;

%%


int main() {
    if (yyparse()) {
        printf("ERROR\n");
    }
    else {
        printf("OK\n");
    }
    return 0;
}