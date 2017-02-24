function [ LK,sups ] = apriori( D,min_sup )
%主要函数
m=length(D);
t=1;
for i=1:m
    D{i}=sort(D{i});
    for j=1:size(D{i},2)
        v(t)=D{i}(j);
        t=t+1;
    end
end
v2=sort(unique(v));
for i=1:length(v2)
    C1(i)={v2(i)};
end
LK=cell(1);
sups=cell(1);
v_sum_sup1=sum_sup(D,C1);
index=find(v_sum_sup1>=min_sup)';
L=C1(index);
LK(1)={L};
sups(1)={v_sum_sup1(index)};
k=2;
while  ~isempty(index)
    h=LK{k-1};
    CK=apriori_gen(h);
    if isempty(CK)
        break;
    end
    v_sum_sup=sum_sup(D,CK);
    index=find(v_sum_sup>=min_sup);
    if isempty(index)
        break;
    end
    L=CK(index);
    LK(k)={L};
    sups(k)={v_sum_sup(index)};
    k=k+1;
    CK=cell(0);
end
b=length(LK);
a=length(LK{b});
for i=1:a
    result(i,:)=LK{b}{i};
end
result
sup=sups{b}
end

function  [CK]= apriori_gen(L)
%连接步
m=length(L);
temp=1;
C=cell(0);
s=length(L{1});
for i=1:m  
    for j=i+1:m
        c=cell(0);  
        if (length(L{j})==1)||(isequal(L{i}(1:s-1),L{j}(1:s-1)))
            c(1:s)=L{i}(1:s);
            c(s+1)=L{j}(s);
            if is_infrequent(c,L)
                C(temp)={c};
                temp=temp+1;
            end
        end
    end
end
CK=C;
end


function [flag]=is_infrequent( c,L )
%先验知识
m1=length(c);
m2=length(L);       
flag=1;
for i=1:m1-2
    flag=0;
    if i==1
        temp(1:m1-1)=c(2:m1);
    else
        temp(1:i-1)=c(1:i-1);
        temp(i:m1-1)=c(i+1:m1);
    end
    for j=1:m2
        if isequal(temp,L{j})
            flag=1;
            break;
        end
    end
    if flag==1
        break;       
    end
 flag=0;     

end
end

function [ v_sum_sup ] = sum_sup( D,C )
%记录支持频数
s=length(C);
v_sum_sup=zeros(s,1);
for i=1:length(D)
    for j=1:length(C)
        if isequal(C{j},intersect(C{j},D{i}))
            v_sum_sup(j)=v_sum_sup(j)+1;
        end
    end    
end

end



