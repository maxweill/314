/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2017                              *
 *  Author: Ulrich Kremer                    *
 *  Student Version                          *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

int main()
{
	Instruction *head,*tail,*ptr;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	ptr=head;
	
	int membuf[2048];
	int registers[1024];
	for(;ptr!=NULL;ptr=ptr->next)
	{
	tail=ptr;	
	}

	ptr=tail;
	tail->critical = 'c';
	int memptr = head->field1;
	
	
	for(;ptr!=NULL;ptr=ptr->prev)
	{
		int x = ptr->opcode;
		
		if(x==0)
		{
			if (registers[ptr->field2]>0)
			{
				ptr->critical='c';
			}
		}
		
		if(x==1)
		{
			if (registers[ptr->field3]>0)
				{
				membuf[memptr+ptr->field2]=1;
				ptr->critical='c';
				}
		}
		
		if(x==2)
		{
			if(registers[ptr->field2]>0 && membuf[memptr+ptr->field3]>0)
			{
				registers[ptr->field1]=1;
				membuf[memptr+ptr->field3]=0;
				ptr->critical='c';
			}
		}
		
		if(x==6 || x==5 || x==4 || x==3)
		{
			if(registers[ptr->field3]>0)
			{
				registers[ptr->field1]=1;
				registers[ptr->field2]=1;
				ptr->critical='c';
			}
		}
		
		if(x == 7)
		{
			registers[ptr->field1]=1;
			membuf[memptr+ptr->field2]=1;
			ptr->critical='c';
		}
	}
	int i=0;
	ptr=head;
	Instruction *curr,*prev,*next;
	
	for(;ptr!=NULL;ptr=ptr->next)
	{
			//printf("segfault not on line %d\n",i);
			if(ptr->critical!='c')
			{
				curr=ptr;
				prev=curr->prev;
				next=curr->next;
				
				prev->next=next;
				next->prev=prev;
			}
			i++;
	}
	

	if (head) 
		PrintInstructionList(stdout, head);
	
	return EXIT_SUCCESS;
}

