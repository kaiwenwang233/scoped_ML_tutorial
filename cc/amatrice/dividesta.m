cd /home/chaos/dschaff/source/correl/preprocess/preprocess/runs/

f='/data/ch1/dschaff/nocal/programs/preprocess/getoldsachdr/preprocessoldhdr/scripts/stacount';
f='/data/ch1/dschaff/nocal/programs/preprocess/preprocess/runs/stacount.old';
f='/data/ch1/dschaff/nocal/programs/preprocess/preprocess/runs/stacount';
f='/home/chaos/dschaff/source/correl/preprocess/preprocess/runs/stacount';
%f='/data/ch1/dschaff/nocal/programs/preprocess/preprocessoldids/runs/stacount';

%f='/home/chaos/dschaff/source/correl/correl9/scripts/stacount';

[stas,ct]=textread(f,'%s %d');

f2='/data/hy71/dschaff/nocal/stas.loc';
[stas2,slat,slon]=textread(f2,'%s %f %f');
i=ismember(stas,stas2);
stas(~i)=[]; ct(~i)=[];



i=ct==0;
stas(i)=[]; ct(i)=[];

[ct,i]=sort(ct);
stas=stas(i);

num=4;
%num=1;
nstas=length(stas);
nrow=floor(nstas/num);
nf=num*nrow;
stanew=reshape(stas(1:num*nrow),num,nrow)';
count=reshape(ct(1:num*nrow),num,nrow)';

for i=1:nstas-nf

s=[stanew(:,i); stas(nf+i)];
printout(sprintf('stas%d',i),'%s\n',{s},'w');

end


for i=nstas-nf+1:num

s=[stanew(:,i)];
printout(sprintf('stas%d',i),'%s\n',{s},'w');

end


fprintf('Total count: %d\n',sum(ct));


