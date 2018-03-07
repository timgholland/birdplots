plot_orders <- function(myeb.area,checklist){

myOrders <- myeb.area %>%
  filter(category != "domestic" | latin_binomial=="Columba livia") %>%
  distinct(latin_binomial,.keep_all=T) %>%
  group_by(order_with_desc) %>%
  summarize(myOrders = n()) %>%
  arrange(desc(myOrders))

orderSum <- checklist %>%
  group_by(order_with_desc) %>%
  summarize(totOrders = n()) %>%
  arrange(desc(totOrders)) %>%
  left_join(myOrders,by="order_with_desc") %>%
  mutate(myOrders = replace(myOrders,is.na(myOrders),0)) %>%
  mutate(order_with_desc=factor(order_with_desc,levels=rev(order_with_desc))) %>%
 # mutate(order=factor(order,levels=rev(order))) %>%
  mutate(IDed = 100*myOrders/totOrders) %>%
  mutate(NotIDed = 100-IDed)

ordersPerc <-  ggplot(data=gather(orderSum,yn,percentage,IDed,NotIDed))+
  geom_col(aes(order_with_desc,percentage,fill=yn),position = position_stack(reverse = TRUE),fill=rep(c(ebPal[5],muteCol(ebPal[5],0.2,0.8)),length(rownames(orderSum)))) +
  coord_flip(ylim=c(0,100),xlim=c(1,length(rownames(orderSum)))) +
  theme(axis.text=element_text(size=figLabelSz), plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) 

ordersRawTrunc <-  ggplot(data=orderSum)+
  geom_col(aes(order_with_desc,totOrders),position = position_stack(reverse = TRUE),fill=muteCol(ebPal[4],0.2,0.8)) +
  geom_col(aes(order_with_desc,myOrders),position = position_stack(reverse = TRUE),fill=ebPal[4]) +
  coord_flip(xlim=c(1,length(rownames(orderSum)))) +
  theme(axis.text.x=element_text(size=figLabelSz),axis.text.y=element_blank(),axis.title.y = element_blank())

title <- ggdraw() + draw_label("Species ID'ed in each order", fontface='bold',size=figTitleSz,hjust=0,x=0)
ordPlots <- plot_grid(ordersPerc + 
                        labs(x="Orders",y = "Percentage") + 
                        theme(axis.title = element_text(size=figAxTitleSz)),
                      ordersRawTrunc + 
                        coord_flip(ylim=c(0,600)) + 
                        labs(y = "Number of species") + 
                        theme(axis.title = element_text(size=figAxTitleSz)),
                      ordersRawTrunc + 
                        #             scale_x_continuous(minor_breaks=NULL,breaks=c(6000,6200,6400)) +
                        coord_flip(ylim=c(6000,6400)) + 
                        theme(plot.margin=margin(0.1,0.1,0.1,0.5,"cm"),axis.ticks = element_blank(),axis.title=element_blank()),
                      align="h",rel_widths = c(0.5,0.3,0.2),nrow=1)
plot_grid(title,ordPlots,align="v",rel_heights=c(0.05,1),ncol=1)
}