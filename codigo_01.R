# Figuras Educação Superior

library("DT")
library("readr")
library("dplyr")
library("ggplot2")
library("kableExtra")
library("tidyr")
library("formattable")
library("plotly")
library("ggplotlyExtra")
library("ggpubr")
#library(tibble)



CENSO_SUP <- NULL
CENSO_SUP <- read_delim("data/educacao/CENSO_SUPERIOR/DM_ALUNO_2009_2018_CO_IES_by_ANO_TP_COR_RACA_TP_SEXO.csv",",",escape_double = FALSE,trim_ws = FALSE)
#CENSO_SUP$TP_SEXO <- ifelse(CENSO_SUP$TP_SEXO==1,"Feminino","Masculino")
CENSO_SUP$TP_SEXO <- ifelse(CENSO_SUP$TP_SEXO==1,"F","M")

# Transformacao da variavel numerica de acordo com o vocabulario 
CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==0,"*NQD",
                                CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==1,"Branca",CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==2,"Preta",CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==3,"Parda",CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==4,"Amarela",CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==5,"Indígena",CENSO_SUP$TP_COR_RACA <- ifelse(CENSO_SUP$TP_COR_RACA==9,"*NDI",NA)))))))

CONCLUINTES_SSA <- CENSO_SUP %>% filter(IN_CONCLUINTE==1)



HEAT_SSA_IES_SEXO_RACA <- CONCLUINTES_SSA %>% dplyr::group_by(ANO_CENSO_SUP,TP_SEXO,TP_COR_RACA) %>% summarise(quantidade = sum(value,na.rm = "TRUE")) 



## COMENTARIO
## A figura a seguir resume as informacoes de duas perguntas realizadas pelas estudantes:
## Qual a porcentagem de mulheres e homens que se formam no ensino superior?
## Dos alunos que se formam no ensino superior, qual a porcentagem de negros e brancos?


png("figure/heat_map_EdSup.png")

#graph_ano_grad_sexo <-
  ggplot(HEAT_SSA_IES_SEXO_RACA, 
                             aes(x = TP_COR_RACA, y = TP_SEXO, fill =quantidade )) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu",direction = 1) +
  xlab(" ") + ylab("") + 
  ggtitle("Concluintes em cursos presenciais de graduação") +
  labs(fill = " ")+
  theme(axis.title.x = element_text(color = "gray",size = 20)) +
  theme(axis.text.x=element_text(color = "black", size=16, angle=90, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "gray",size = 20))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  facet_wrap(~ANO_CENSO_SUP,ncol = 2)  

#ggplotly(graph_ano_grad_sexo)

dev.off()

## COMENTARIO  
## As mesma pergunta anterior removendo informacoes de NDI e NQD, e apenas as informacoes de
## de sexo e preta e parda


#target = c("Amarela","Branca","Indígena","Parda","Preta")
target = c("Parda","Preta")
HEAT_SSA_IES_SEXO_RACA_S_NQD_NDI <- HEAT_SSA_IES_SEXO_RACA %>%  filter(TP_COR_RACA %in% target) %>%
  dplyr::group_by(ANO_CENSO_SUP,TP_SEXO,TP_COR_RACA) %>% 
  summarise(quantidadef = sum(quantidade,na.rm = "TRUE")) 




png("figure/heat_map_EdSup_SEM_NDI_NQD.png")

#graph_ano_grad_sexo <-
HEAT_SSA_IES_SEXO_RACA_S_NQD_NDI %>% 
  ggplot(aes(x = TP_COR_RACA, y = TP_SEXO, fill =quantidadef )) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu",direction = 1) +
  xlab(" ") + ylab("") + 
  ggtitle("Concluintes em cursos presenciais de graduação exceto NQD e NQI") +
  labs(fill = " ")+
  theme(axis.title.x = element_text(color = "gray",size = 20)) +
  theme(axis.text.x=element_text(color = "black", size=16, angle=90, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "gray",size = 20))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  facet_wrap(~ANO_CENSO_SUP,ncol = 2)  

#ggplotly(graph_ano_grad_sexo)

dev.off()


png("figure/heat_map_EdSup_SEM_NDI_NQD.png")

#graph_ano_grad_sexo <-
ggplot(HEAT_SSA_IES_SEXO_RACA_S_NQD_NDI, 
       aes(x = TP_COR_RACA, y = TP_SEXO, fill =quantidadef )) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu",direction = 1) +
  xlab(" ") + ylab("") + 
  ggtitle("Concluintes em cursos presenciais de graduação exceto NQD e NQI") +
  labs(fill = " ")+
  theme(axis.title.x = element_text(color = "gray",size = 20)) +
  theme(axis.text.x=element_text(color = "black", size=16, angle=90, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "gray",size = 20))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  facet_wrap(~ANO_CENSO_SUP,ncol = 2)  

#ggplotly(graph_ano_grad_sexo)

dev.off()

## COMENTARIO  
## Mesma informacoes da anterior mas em gráfico de linha


png("figure/line_plot_EdSup_SEM_NDI_NQD.png")

#graph_ano_grad_sexo <-
ggplot(HEAT_SSA_IES_SEXO_RACA_S_NQD_NDI, 
       aes(x = ANO_CENSO_SUP , y = quantidadef, fill =TP_SEXO )) +
  geom_line(aes(linetype = TP_SEXO ,color=TP_SEXO))+
  geom_point(aes(color=TP_SEXO))+
  scale_color_brewer(palette="Paired") +
  xlab(" ") + ylab("") + 
#    geom_line(linetype = "dashed")+
#  geom_point()+
#  xlab(" ") + ylab("") + 
  ggtitle("Concluintes em cursos presenciais de graduação exceto NQD e NQI") +
  labs(fill = " ")+
  theme(axis.title.x = element_text(color = "gray",size = 20)) +
  theme(axis.text.x=element_text(color = "black", size=16, angle=90, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "gray",size = 20))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.title=element_blank()) +
  #theme(legend.text = element_text(size = 14)) +
  #theme(legend.title = element_text(size = 16)) +
  facet_wrap(~TP_COR_RACA,ncol = 2)  


#ggplotly(graph_ano_grad_sexo)

dev.off()



tiff("figure/line_plot_EdSup_COM_NDI_NQD.tiff")

#graph_ano_grad_sexo <-
#ggplot(HEAT_SSA_IES_SEXO_RACA, 
ggplot(HEAT_SSA_IES_SEXO_RACA_S_NQD_NDI, 
      aes(x = ANO_CENSO_SUP , y = quantidadef))+  #, group=TP_SEXO, color =TP_SEXO )) +
  geom_line(aes(linetype = TP_SEXO ,color=TP_SEXO))+
  geom_point(aes(color=TP_SEXO))+
  scale_color_brewer(palette="Paired") +
  xlab(" ") + ylab("") + 
  ggtitle("Concluintes em cursos presenciais de graduação em Salvador-BA") +
#  labs( = " ")+
  theme(axis.title.x = element_text(color = "gray",size = 20)) +
  theme(axis.text.x=element_text(color = "black", size=16, angle=90, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "gray",size = 20))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.title=element_blank()) +
  #theme(legend.text = element_text(size = 14)) +
  #theme(legend.title = element_text(size = 16)) +
  facet_wrap(~TP_COR_RACA,ncol = 2)  

#ggplotly(graph_ano_grad_sexo)

dev.off()






