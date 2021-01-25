#Sigara İçmeyen Grup

cda1=-c(2700,2850,3020,2950,3240,2900,3300,2850,2500,2960,3400,3260,3700,3120,3350,3290,3860,2740,2780,
        3410,2750,3240,3170,2890,3500,2970,2987,3421,2986,3000,3214,3621,2941,2450,3598)
bc1=-c(36.5,35.9,37,36.5,36,36.2,37.5,35.8,35,36.4,37.2,36.6,38.3,35.9,37,37.2,38.4,36.7,34.2,36,35.2,
       36,37.2,36.2,36,35.9,37,39,37.2,38.6,37.1,36.8,37,38,39)
#Sigara İçen Grup
cda2=-c(2600,2460,2010,3000,1970,2730,2350,2100,2330,2180,2645,1860,2440,2120,2090,2310,3100,2860,2200,2040,
        2100,2400,2365,3100,2510,2947,2875,2654,2987,2100,2350,2241,2546,2389,2000)
bc2=-c(34,34.5,33,35.6,32.8,34.6,33.2,32.7,34.7,33.4,36.1,32,32.5,32,33.5,33.5,37.4,35.4,34.2,33.1,
       35.4,36.2,33.2,31.5,34.5,32,33.5,31.8,34,32,33.2,32.5,33.8,31.7,31.8)
n1=35
n2=35
cda=-c(cda1,cda2)
bc=-c(bc1,bc2)
faktor= -c(rep(-1,n1),rep(-2,n2))
Data= cbind(cda,bc,faktor)
Data=as.data.frame(data)
data

cov(data)
cor(data)
plot(data)

#VERİLERİN NORMAL DAĞILIMDAN GELMESİNİN KONTROLÜ

#Sigara içmeyen grup için 
H0: Veriler Normal Dağılımdan Gelmektedir
H1: Veriler Normal Dağılımdan Gelmemektedir
shapiro.test(cda1)
#p=0.87 > alpha=0.05 H0 Reddedilemez Veriler Normal Dağılımdan Gelmektedir
shapiro.test(bc1)
#p=0.32 > alpha=0.05 H0 Reddedilemez Veriler Normal Dağılımdan Gelmektedir

#Sigara içen grup için 
H0: Veriler Normal Dağılımdan Gelmektedir
H1: Veriler Normal Dağılımdan Gelmemektedir
shapiro.test(cda2)
#p=0.07 > alpha=0.05 H0 Reddedilemez Veriler Normal Dağılımdan Gelmektedir

shapiro.test(bc2)
#p=0.1 > alpha=0.05 H0 Reddedilemez Veriler Normal Dağılımdan Gelmektedir

#Çok Değişkenli Normallik Testi
mvn(data)





#HOTELLİNG T2 TESTİ İÇİN GEREKLİ NORMALLİK VARSAYIMI SAĞLANDIĞI İÇİN TESTE GEÇİLEBİLİR

H0: İki grup icin ortalamalar birbirine esittir
H1: İki grup ortalaması birbirinden farklıdır
H0: μx = μy
H1: μx != μy

?Hotelling

model= hotelling.test(cbind(cda,bc) ~ as.factor(faktor),data=Data,alpha=0.05)
model

# Model yorum: p-value < alpha olduğundan H0 hipotezi reddedilir.
# İki grup ortalaması arasında farklılık bulunur. 
#Farklılık hangi değişkenden kaynaklanıyor araştırmak gerekir.





