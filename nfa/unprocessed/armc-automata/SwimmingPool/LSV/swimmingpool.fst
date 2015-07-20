model swimmingpool {

var  x1,x2,x3,x4,x5,x6,x7,p1,p2; 

states normal;


transition t1 := {
	from := normal;
	to := normal;
        guard := x6>=1;
	action :=
		x1'=x1+1,
		x6'=x6-1;
};


transition t2 := {
	from :=normal;
	to := normal;
         guard :=x1>=1 && x7>=1;
	action :=
		x2'=x2+1,
		x1'=x1-1,
		x7'=x7-1;
};

transition t3   := {
	from := normal;
	to := normal;
         guard :=x2>=1;
	action:= x6'=x6+1,x3'=x3+1,x2'=x2-1;
};

transition t4   := {
	from := normal;
	to := normal;
         guard :=
                 x3>=1 && x6>=1;	
	action:=
	x4'=x4+1,
	x3'=x3-1,
	x6'=x6-1;
};

transition t5   := {
	from := normal;
	to := normal;
         guard :=
	x4>=1;
	action:=
	x5'=x5+1,
	x7'=x7+1,
	x4'=x4-1;
};

transition t6   := {
  from := normal;
  to := normal;
  guard:= x5>=1;
  action:=
    x6'=x6+1,
    x5'=x5-1;	
};

}

strategy s1 {

setMaxState(0);
setMaxAcc(100);

Region omega := {true}; 

Region init :=
	{state=normal && x1=0 && x2=0 && x3=0 && x4=0 && x5=0 && x6=p1 && x7=p2 && p1>=0 && p2>=0};

Transitions t := {t1,t2,t3,t4,t5,t6};

Region reach := post*(init, t,4);

Region deadlock := {
	x6=0 &&    
	(x1=0 || x7=0) &&
	x2=0 &&
	(x3=0 || x6=0) &&
	x4=0 &&
	x5=0 		
};




Region result := reach && deadlock;


if (isEmpty(result)) 
	then print(" safety holds for all parameters ");
	else print(" unsafe ");
endif

}
