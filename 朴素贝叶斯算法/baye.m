function [ result class_tag] = baye(train,class_vector,x)
class_tag=unique(class_vector);
l=length(class_tag);
m=length(class_vector);
a=zeros(l,1);
%«ÛP£®CI£©
for i=1:l
    for j=1:m
        if isequal(class_tag(i),class_vector(j))
            a(i)=a(i)+1;
        end
    end
    P(i)=a(i)/m;
end

for i=1:length(x)
    for j=1:l
        [x1,y1]=find(strcmp(train,x{i}));
        [x2,y2]=find(strcmp(class_vector,class_tag{j}));
        q(j,i)=length(intersect(x1,x2))/a(j);
    end
end
[m,n]=size(q);
px=ones(m,1);
result=ones(m,1);
for i=1:m
    for j=1:n
        px(i,1)=px(i,1)*q(i,j);
    end
    result(i)=px(i,1)*P(i);
end
   
end

