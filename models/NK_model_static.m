% This code solves the static New Keynesian model with shocks to expectations 
% appearing in Adams and Barrett (2024) "Shocks to Inflation Expectations"
%
% The creates the plots appearing in Section 2: "Motivating Model"
% and Appendix B: "Extensions to the Motivating Model"


% Parameters for the basic AS-AD graph:
beta = .99;
kappa = .2;
phi_pi = 1.5; phi_y = .0125; gamma = 1; phi_pi_future = .5;

yvecmin = -1;
yvecmax = 1;

yvec = [yvecmin:.01:yvecmax; yvecmin:.01:yvecmax]';
zvec = [zeros(size(yvec,1),1) ones(size(yvec,1),1)];

%aggregate supply pre/post sentiment
ass_vec = beta*zvec + kappa*yvec;

%aggregate demand pre/post sentiment
add_vec = 1/phi_pi*zvec - (phi_y+gamma)/phi_pi*yvec;


pimin = min(min(min(add_vec)),min(min(ass_vec)));

pimax = max(max(max(add_vec)),max(max(ass_vec)));


%Figure 1 (a):

close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,1),add_vec(end,1),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_preshock.png')
hold off

%Figure 1 (b):
grayshade = .75;
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,2),add_vec(:,2),'LineWidth',2,'Color','b','Linestyle','--')
plot(yvec(:,2),ass_vec(:,2),'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,2),add_vec(end,2),'AD','FontSize',14)
text(yvec(end,2),ass_vec(end,2),'AS','FontSize',14)
annotation('arrow',[.52 .39],[.4 .7],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_postshock.png')
hold off

%Now an economy without the monetary policy response!

%Figure 12 (a):
close all
hold on
plot([0 0],[pimin pimax],'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(0,pimin,'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_nofed_preshock.png')
hold off

grayshade = .75;
zetafudge = .3;

%Figure 12 (b):
close all
hold on
plot([0 0],[pimin pimax],'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot([zetafudge zetafudge],[pimin pimax],'LineWidth',2,'Color','b','Linestyle','--')
plot(yvec(:,2),ass_vec(:,2),'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(zetafudge,pimin,'AD','FontSize',14)
text(yvec(end,2),ass_vec(end,2),'AS','FontSize',14)
annotation('arrow',[.52 .61],[.4 .75],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_nofed_postshock.png')
hold off


add_vec_nofed = 1/phi_pi*zvec - (phi_y+gamma)/phi_pi*yvec;

% Now we will plot the effects of different folks receiving sentiments

%agg demand for housheold only sentiment is add_vec(:,2)
%agg supply for firm only sentiment is ass_vec(:,2), but I am going to cut
%the change in half to make the grpah nicer:
ass_firmsenti_vec = .5*ass_vec(:,1) + .5*ass_vec(:,2);
%aggregate demand if only fed observes sentiment
add_fedsenti_vec = - phi_pi_future  - (phi_y+gamma)/phi_pi*yvec(:,2);


%Figure 13 (a) Sentiment Shock to Firms:
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,2),ass_firmsenti_vec,'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,1),add_vec(end,1),'AD','FontSize',14)
text(yvec(end,2),ass_firmsenti_vec(end),'AS','FontSize',14)
annotation('arrow',[.55 .40],[.4 .53],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_firm_senti.png')
hold off

%Figure 13 (b) Sentiment Shock to Households:
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
plot(yvec(:,2),add_vec(:,2),'LineWidth',2,'Color','b','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,2),add_vec(end,2),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
annotation('arrow',[.52 .72],[.41 .45],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_hh_senti.png')
hold off

%find that x-intercept for the label
add_fedsenti_vec
[uhh xinterceptdex] = min(abs(add_fedsenti_vec-pimin));

%Figure 13 (c) Sentiment Shock to Central Bank:
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
plot(yvec(:,2),add_fedsenti_vec,'LineWidth',2,'Color','b','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(xinterceptdex+7,2),add_fedsenti_vec(xinterceptdex),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
annotation('arrow',[.49 .32],[.39 .36],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_fed_senti.png')
hold off

