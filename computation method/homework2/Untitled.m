for x0=0:0.01:64
    i=1;
    x=x0(i);
    i=i+1;
    y1=L8(x);
    y2=s1(x);
    y3=sqrt(x);
    plot(x,y1,'r');
    hold on
    plot(x,y2,'g');
    hold on
    plot(x,y3,'b');
    hold on
end
%����ͼ�еĿ�֪�����ߵ�������Բ������ߵ���ϣ���������������ֵ����ϱ��������ղ�ֵ����ϸ���׼%