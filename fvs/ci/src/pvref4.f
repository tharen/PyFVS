      SUBROUTINE PVREF4 (KARD2,LPVCOD,LPVREF)
      IMPLICIT NONE
C----------
C  **PVREF4--CI   DATE OF LAST REVISION: 04/12/11
C----------
C
C     MAPS PV/REFERENCE CODES INTO A FVS HABITAT/ECOCLASS CODE
C     CALLED FROM **HABTYP** WHEN CPVREF IS GREATER THAN ZERO
C
C     INPUT VARIABLES
C     KARD2          - FIELD 2 OF STDINFO KEYWORD
C     CPVREF         - FIELD 7 OF STDINFO KEYWORD
C                    - CARRIED IN PLOT.F77
C
C     RETURN VARIABLES
C     KARD2          - MAPPED FVS HABITAT/ECOCLASS CODE
C
C     INTERNAL VARIABLES
C     PVCODE,PVREF   - ARRAYS OF PV CODE/REFERENCE CODE COMBINATIONS
C                      FROM FSVEG DATA BASE
C     HABPVR         - FVS HABITAT/ECOCLASS CODE CORRESPONDING TO
C                      PV CODE/REFERENCE CODE COMBINATION
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
COMMONS
C----------
C  DECLARATIONS
C----------
      INTEGER      I,NCODES
      PARAMETER    (NCODES=827)
      CHARACTER*10 PVREF(NCODES),PVCODE(NCODES),KARD2
      CHARACTER*10 HABPVR(NCODES)
      LOGICAL LPVCOD,LPVREF
C----------
C  DATA STATEMENTS
C----------
      DATA (HABPVR(I),I=   1,  60)/
     &'050       ','050       ','050       ','050       ',
     &'050       ','050       ','050       ','050       ',
     &'050       ','060       ','060       ','060       ',
     &'060       ','070       ','070       ','070       ',
     &'080       ','080       ','080       ','100       ',
     &'100       ','100       ','100       ','100       ',
     &'100       ','100       ','100       ','100       ',
     &'100       ','100       ','120       ','130       ',
     &'140       ','140       ','140       ','140       ',
     &'140       ','140       ','160       ','160       ',
     &'160       ','162       ','161       ','170       ',
     &'190       ','195       ','195       ','195       ',
     &'200       ','200       ','200       ','200       ',
     &'210       ','210       ','220       ','220       ',
     &'220       ','221       ','221       ','221       '/
      DATA (HABPVR(I),I=  61, 120)/
     &'222       ','250       ','260       ','260       ',
     &'260       ','260       ','260       ','260       ',
     &'260       ','260       ','260       ','262       ',
     &'264       ','265       ','265       ','265       ',
     &'280       ','280       ','280       ','280       ',
     &'290       ','290       ','310       ','310       ',
     &'310       ','313       ','313       ','313       ',
     &'315       ','320       ','320       ','320       ',
     &'320       ','323       ','323       ','323       ',
     &'324       ','325       ','325       ','330       ',
     &'330       ','331       ','332       ','334       ',
     &'334       ','340       ','340       ','340       ',
     &'341       ','341       ','341       ','343       ',
     &'343       ','343       ','344       ','360       ',
     &'360       ','360       ','360       ','360       '/
      DATA (HABPVR(I),I= 121, 180)/
     &'370       ','370       ','370       ','371       ',
     &'371       ','371       ','372       ','372       ',
     &'372       ','375       ','375       ','375       ',
     &'375       ','375       ','380       ','380       ',
     &'380       ','380       ','380       ','380       ',
     &'385       ','385       ','385       ','385       ',
     &'385       ','390       ','390       ','390       ',
     &'390       ','390       ','392       ','393       ',
     &'395       ','395       ','395       ','395       ',
     &'395       ','396       ','396       ','396       ',
     &'396       ','396       ','397       ','397       ',
     &'397       ','397       ','398       ','398       ',
     &'398       ','398       ','398       ','398       ',
     &'398       ','400       ','400       ','400       ',
     &'400       ','400       ','400       ','400       '/
      DATA (HABPVR(I),I= 181, 240)/
     &'400       ','400       ','400       ','400       ',
     &'410       ','410       ','410       ','410       ',
     &'410       ','410       ','410       ','410       ',
     &'410       ','410       ','410       ','410       ',
     &'410       ','410       ','440       ','440       ',
     &'440       ','490       ','490       ','490       ',
     &'493       ','493       ','493       ','500       ',
     &'505       ','510       ','511       ','515       ',
     &'520       ','525       ','526       ','527       ',
     &'580       ','580       ','580       ','580       ',
     &'580       ','580       ','580       ','580       ',
     &'580       ','580       ','580       ','580       ',
     &'580       ','580       ','580       ','580       ',
     &'580       ','580       ','580       ','580       ',
     &'580       ','585       ','590       ','591       '/
      DATA (HABPVR(I),I= 241, 300)/
     &'592       ','593       ','600       ','600       ',
     &'600       ','600       ','600       ','600       ',
     &'600       ','600       ','600       ','600       ',
     &'600       ','600       ','600       ','600       ',
     &'600       ','600       ','600       ','600       ',
     &'600       ','600       ','600       ','600       ',
     &'605       ','620       ','621       ','625       ',
     &'635       ','635       ','635       ','635       ',
     &'636       ','636       ','636       ','637       ',
     &'638       ','640       ','640       ','640       ',
     &'640       ','640       ','645       ','645       ',
     &'645       ','645       ','645       ','645       ',
     &'650       ','650       ','650       ','650       ',
     &'651       ','651       ','651       ','652       ',
     &'654       ','654       ','654       ','655       '/
      DATA (HABPVR(I),I= 301, 360)/
     &'655       ','655       ','660       ','660       ',
     &'660       ','661       ','661       ','661       ',
     &'662       ','663       ','663       ','663       ',
     &'670       ','670       ','670       ','671       ',
     &'671       ','671       ','672       ','690       ',
     &'690       ','690       ','691       ','691       ',
     &'691       ','692       ','692       ','692       ',
     &'694       ','705       ','705       ','705       ',
     &'705       ','705       ','720       ','720       ',
     &'720       ','720       ','720       ','720       ',
     &'720       ','721       ','721       ','721       ',
     &'723       ','723       ','723       ','730       ',
     &'730       ','730       ','730       ','730       ',
     &'730       ','731       ','731       ','731       ',
     &'732       ','732       ','732       ','732       '/
      DATA (HABPVR(I),I= 361, 420)/
     &'734       ','734       ','734       ','740       ',
     &'745       ','745       ','745       ','745       ',
     &'745       ','750       ','750       ','750       ',
     &'750       ','750       ','750       ','780       ',
     &'780       ','780       ','780       ','780       ',
     &'780       ','780       ','780       ','790       ',
     &'790       ','790       ','790       ','790       ',
     &'790       ','790       ','790       ','790       ',
     &'790       ','790       ','790       ','790       ',
     &'790       ','790       ','790       ','790       ',
     &'790       ','790       ','790       ','790       ',
     &'790       ','790       ','790       ','790       ',
     &'791       ','791       ','791       ','793       ',
     &'810       ','810       ','810       ','810       ',
     &'810       ','810       ','810       ','810       '/
      DATA (HABPVR(I),I= 421, 480)/
     &'810       ','810       ','810       ','810       ',
     &'810       ','810       ','830       ','830       ',
     &'830       ','831       ','831       ','831       ',
     &'833       ','850       ','870       ','870       ',
     &'870       ','870       ','870       ','870       ',
     &'870       ','870       ','870       ','870       ',
     &'870       ','900       ','900       ','900       ',
     &'900       ','900       ','900       ','900       ',
     &'900       ','900       ','900       ','900       ',
     &'900       ','900       ','900       ','900       ',
     &'900       ','920       ','920       ','920       ',
     &'920       ','940       ','940       ','940       ',
     &'940       ','955       ','955       ','955       ',
     &'955       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       '/
      DATA (HABPVR(I),I= 481, 540)/
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       '/
      DATA (HABPVR(I),I= 541, 600)/
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       '/
      DATA (HABPVR(I),I= 601, 660)/
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       '/
      DATA (HABPVR(I),I= 661, 720)/
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       '/
      DATA (HABPVR(I),I= 721, 780)/
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       '/
      DATA (HABPVR(I),I= 781, NCODES)/
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       ','999       ',
     &'999       ','999       ','999       '/
      DATA (PVCODE(I),I=   1,  60)/
     &'010       ','050       ','051       ','41010     ',
     &'41010     ','41010     ','41045     ','41050     ',
     &'41050     ','060       ','41060     ','41060     ',
     &'41060     ','070       ','41070     ','41070     ',
     &'080       ','41080     ','41080     ','41100     ',
     &'41100     ','41100     ','41107     ','41108     ',
     &'41109     ','41111     ','41112     ','41113     ',
     &'41114     ','41115     ','41120     ','41130     ',
     &'41140     ','41140     ','41140     ','41141     ',
     &'41143     ','41144     ','41160     ','41160     ',
     &'41160     ','41162     ','41161     ','41170     ',
     &'41190     ','41195     ','41195     ','41195     ',
     &'200       ','41200     ','41200     ','41200     ',
     &'41210     ','41210     ','220       ','41220     ',
     &'41220     ','221       ','41221     ','41221     '/
      DATA (PVCODE(I),I=  61, 120)/
     &'41222     ','41250     ','260       ','266       ',
     &'270       ','41260     ','41260     ','41260     ',
     &'41260     ','41266     ','41266     ','41262     ',
     &'41264     ','265       ','41265     ','41265     ',
     &'280       ','41280     ','41280     ','281       ',
     &'41290     ','41290     ','310       ','41310     ',
     &'41310     ','313       ','41313     ','41313     ',
     &'41315     ','320       ','41320     ','41320     ',
     &'41320     ','323       ','41323     ','41323     ',
     &'41324     ','325       ','41325     ','41330     ',
     &'41330     ','41331     ','41332     ','41334     ',
     &'41334     ','340       ','41340     ','41340     ',
     &'341       ','41341     ','41341     ','343       ',
     &'41343     ','41343     ','41344     ','360       ',
     &'41360     ','41360     ','41365     ','41366     '/
      DATA (PVCODE(I),I= 121, 180)/
     &'370       ','41370     ','41370     ','371       ',
     &'41371     ','41371     ','372       ','41372     ',
     &'41372     ','375       ','41375     ','41375     ',
     &'41375     ','41376     ','380       ','41380     ',
     &'41380     ','41380     ','41380     ','41382     ',
     &'385       ','41385     ','41385     ','41385     ',
     &'41385     ','390       ','391       ','41390     ',
     &'41390     ','41390     ','41392     ','41393     ',
     &'395       ','41395     ','41395     ','41395     ',
     &'41395     ','396       ','41396     ','41396     ',
     &'41396     ','41396     ','397       ','41397     ',
     &'41397     ','41397     ','398       ','399       ',
     &'41398     ','41398     ','41398     ','41399     ',
     &'41399     ','400       ','415       ','495       ',
     &'41400     ','41400     ','41400     ','41415     '/
      DATA (PVCODE(I),I= 181, 240)/
     &'41415     ','43002     ','43004     ','43006     ',
     &'410       ','430       ','470       ','475       ',
     &'485       ','497       ','41410     ','41410     ',
     &'41410     ','41485     ','41485     ','41497     ',
     &'41497     ','43012     ','440       ','41440     ',
     &'41440     ','490       ','41490     ','41490     ',
     &'493       ','41493     ','41493     ','41500     ',
     &'41505     ','41510     ','41511     ','41515     ',
     &'41520     ','41525     ','41526     ','41527     ',
     &'41580     ','41800     ','41800     ','41861     ',
     &'41861     ','41861     ','41862     ','41863     ',
     &'41863     ','41863     ','41864     ','41865     ',
     &'41865     ','41865     ','41866     ','41867     ',
     &'41868     ','41869     ','41871     ','41872     ',
     &'41873     ','41585     ','41590     ','41591     '/
      DATA (PVCODE(I),I= 241, 300)/
     &'41592     ','41593     ','600       ','601       ',
     &'603       ','707       ','760       ','761       ',
     &'762       ','41600     ','41600     ','41600     ',
     &'41601     ','41601     ','41603     ','41603     ',
     &'41603     ','41707     ','41707     ','41708     ',
     &'41709     ','41746     ','41760     ','41760     ',
     &'41605     ','41620     ','41621     ','41625     ',
     &'635       ','41635     ','41635     ','41635     ',
     &'636       ','41636     ','41636     ','41637     ',
     &'41638     ','41416     ','41640     ','41640     ',
     &'41640     ','41640     ','645       ','647       ',
     &'41645     ','41645     ','41645     ','41645     ',
     &'650       ','41650     ','41650     ','41650     ',
     &'651       ','41651     ','41651     ','41652     ',
     &'654       ','41654     ','41654     ','655       '/
      DATA (PVCODE(I),I= 301, 360)/
     &'41655     ','41655     ','660       ','41660     ',
     &'41660     ','661       ','41661     ','41661     ',
     &'41662     ','663       ','41663     ','41663     ',
     &'670       ','41670     ','41670     ','671       ',
     &'41671     ','41671     ','41672     ','690       ',
     &'41690     ','41690     ','691       ','41691     ',
     &'41691     ','692       ','41692     ','41692     ',
     &'41694     ','607       ','609       ','705       ',
     &'41705     ','41705     ','720       ','722       ',
     &'41720     ','41720     ','41720     ','41720     ',
     &'41747     ','721       ','41721     ','41721     ',
     &'723       ','41723     ','41723     ','730       ',
     &'41725     ','41726     ','41730     ','41730     ',
     &'41730     ','731       ','41731     ','41731     ',
     &'732       ','41732     ','41732     ','41732     '/
      DATA (PVCODE(I),I= 361, 420)/
     &'734       ','41734     ','41734     ','41740     ',
     &'745       ','41745     ','41745     ','41745     ',
     &'41745     ','750       ','751       ','752       ',
     &'41750     ','41750     ','41750     ','701       ',
     &'780       ','781       ','782       ','783       ',
     &'784       ','41780     ','41780     ','702       ',
     &'703       ','704       ','790       ','795       ',
     &'41702     ','41702     ','41702     ','41703     ',
     &'41703     ','41703     ','41704     ','41714     ',
     &'41714     ','41714     ','41715     ','41716     ',
     &'41717     ','41718     ','41718     ','41790     ',
     &'41790     ','41790     ','41795     ','41795     ',
     &'791       ','41791     ','41791     ','41793     ',
     &'810       ','811       ','812       ','41810     ',
     &'41810     ','41810     ','41810     ','41811     '/
      DATA (PVCODE(I),I= 421, 480)/
     &'41811     ','41811     ','41813     ','41814     ',
     &'41815     ','41816     ','830       ','41830     ',
     &'41830     ','831       ','41831     ','41831     ',
     &'41833     ','41850     ','870       ','875       ',
     &'880       ','885       ','886       ','887       ',
     &'891       ','895       ','896       ','897       ',
     &'41870     ','900       ','930       ','945       ',
     &'950       ','960       ','965       ','970       ',
     &'975       ','41900     ','41900     ','41905     ',
     &'41956     ','41957     ','41960     ','41960     ',
     &'41970     ','935       ','41920     ','41920     ',
     &'41920     ','940       ','41940     ','41940     ',
     &'41940     ','955       ','41915     ','41955     ',
     &'41955     ','31        ','35        ','42        ',
     &'43        ','44        ','51        ','54        '/
      DATA (PVCODE(I),I= 481, 540)/
     &'55        ','56        ','59        ','60        ',
     &'62        ','64        ','65        ','73        ',
     &'77        ','78        ','90        ','402       ',
     &'451       ','452       ','453       ','501       ',
     &'502       ','601       ','602       ','721       ',
     &'722       ','723       ','990       ','4605      ',
     &'5600      ','40215     ','41305     ','41305     ',
     &'41367     ','41406     ','41407     ','41407     ',
     &'41407     ','41408     ','41409     ','42000     ',
     &'42001     ','42002     ','42003     ','42004     ',
     &'42005     ','42006     ','42007     ','42008     ',
     &'42009     ','42010     ','42011     ','42012     ',
     &'42020     ','42021     ','42022     ','42023     ',
     &'42040     ','42041     ','42042     ','42043     ',
     &'42044     ','42045     ','42046     ','42047     '/
      DATA (PVCODE(I),I= 541, 600)/
     &'42048     ','42049     ','42050     ','42051     ',
     &'42052     ','42053     ','42054     ','42055     ',
     &'42056     ','42080     ','42081     ','42082     ',
     &'42083     ','42084     ','42085     ','42086     ',
     &'42087     ','42088     ','42089     ','42100     ',
     &'42101     ','42102     ','42103     ','42104     ',
     &'42105     ','42106     ','42107     ','42108     ',
     &'42109     ','42110     ','42111     ','42200     ',
     &'42201     ','42202     ','42203     ','42204     ',
     &'42300     ','42301     ','42302     ','42303     ',
     &'42304     ','42400     ','42401     ','42402     ',
     &'42403     ','42500     ','42600     ','42700     ',
     &'43000     ','43001     ','43001     ','43003     ',
     &'43005     ','43007     ','43008     ','43009     ',
     &'43010     ','43010     ','43011     ','43013     '/
      DATA (PVCODE(I),I= 601, 660)/
     &'43041     ','43042     ','43043     ','43043     ',
     &'43043     ','43044     ','43045     ','43046     ',
     &'43046     ','43046     ','43081     ','43082     ',
     &'43101     ','43101     ','43102     ','43103     ',
     &'43103     ','43104     ','43104     ','43105     ',
     &'43115     ','43151     ','43151     ','43152     ',
     &'43152     ','43155     ','43155     ','43201     ',
     &'43202     ','43202     ','43203     ','43203     ',
     &'43203     ','43204     ','43204     ','43204     ',
     &'43205     ','43205     ','43205     ','43206     ',
     &'43206     ','43206     ','43207     ','43207     ',
     &'43207     ','43208     ','43209     ','43210     ',
     &'43221     ','43222     ','43222     ','43222     ',
     &'43223     ','43223     ','43223     ','43224     ',
     &'43224     ','43224     ','43225     ','43226     '/
      DATA (PVCODE(I),I= 661, 720)/
     &'43227     ','43228     ','43228     ','43241     ',
     &'43242     ','43242     ','43242     ','43243     ',
     &'43243     ','43244     ','43245     ','43271     ',
     &'43281     ','43283     ','43286     ','43301     ',
     &'43301     ','43301     ','43302     ','43302     ',
     &'43302     ','43303     ','43305     ','43306     ',
     &'43306     ','43306     ','43307     ','43308     ',
     &'43308     ','43308     ','43321     ','43322     ',
     &'43323     ','43324     ','43326     ','43353     ',
     &'43353     ','43353     ','43354     ','43400     ',
     &'43551     ','43551     ','43551     ','43552     ',
     &'43553     ','43553     ','43553     ','43601     ',
     &'43602     ','43603     ','43604     ','43604     ',
     &'43604     ','43652     ','43653     ','43801     ',
     &'43801     ','43801     ','43802     ','43804     '/
      DATA (PVCODE(I),I= 721, 780)/
     &'43805     ','43805     ','43806     ','43807     ',
     &'43807     ','43808     ','43808     ','43808     ',
     &'43809     ','43809     ','43809     ','43810     ',
     &'43812     ','43812     ','43812     ','43813     ',
     &'43813     ','43815     ','43821     ','43821     ',
     &'43831     ','43831     ','43831     ','43851     ',
     &'43861     ','43871     ','43871     ','43871     ',
     &'43881     ','43882     ','43882     ','43882     ',
     &'43901     ','43907     ','43921     ','43921     ',
     &'43925     ','43931     ','43931     ','43931     ',
     &'43932     ','43941     ','46000     ','46001     ',
     &'46002     ','46003     ','46011     ','46021     ',
     &'46031     ','46041     ','46042     ','46043     ',
     &'46051     ','46051     ','46051     ','46061     ',
     &'46100     ','46101     ','46111     ','46112     '/
      DATA (PVCODE(I),I= 781, NCODES)/
     &'46113     ','46113     ','46114     ','46114     ',
     &'46131     ','46131     ','46132     ','46133     ',
     &'46133     ','46151     ','46152     ','46153     ',
     &'46154     ','46155     ','46156     ','46157     ',
     &'46158     ','46159     ','46171     ','46172     ',
     &'46173     ','46174     ','46175     ','46191     ',
     &'46192     ','46200     ','46201     ','46201     ',
     &'46202     ','46300     ','46301     ','46301     ',
     &'46632     ','47000     ','47001     ','47002     ',
     &'47011     ','47012     ','47021     ','47021     ',
     &'47025     ','47025     ','47120     ','47126     ',
     &'47145     ','402041    ','431151    '/
      DATA (PVREF(I),I=   1,  60)/
     &'402       ','402       ','402       ','401       ',
     &'403       ','404       ','403       ','401       ',
     &'413       ','402       ','401       ','403       ',
     &'413       ','402       ','401       ','413       ',
     &'402       ','401       ','413       ','401       ',
     &'403       ','404       ','404       ','404       ',
     &'404       ','404       ','404       ','404       ',
     &'404       ','403       ','401       ','401       ',
     &'401       ','403       ','413       ','403       ',
     &'403       ','403       ','401       ','404       ',
     &'413       ','401       ','401       ','401       ',
     &'401       ','401       ','404       ','413       ',
     &'402       ','401       ','403       ','404       ',
     &'401       ','413       ','402       ','401       ',
     &'413       ','402       ','401       ','413       '/
      DATA (PVREF(I),I=  61, 120)/
     &'401       ','401       ','402       ','402       ',
     &'402       ','401       ','403       ','404       ',
     &'413       ','403       ','413       ','401       ',
     &'401       ','402       ','401       ','413       ',
     &'402       ','401       ','413       ','402       ',
     &'401       ','413       ','402       ','401       ',
     &'413       ','402       ','401       ','413       ',
     &'401       ','402       ','401       ','403       ',
     &'413       ','402       ','401       ','413       ',
     &'401       ','402       ','401       ','401       ',
     &'413       ','401       ','401       ','401       ',
     &'413       ','402       ','401       ','413       ',
     &'402       ','401       ','413       ','402       ',
     &'401       ','413       ','401       ','402       ',
     &'401       ','413       ','404       ','404       '/
      DATA (PVREF(I),I= 121, 180)/
     &'402       ','401       ','413       ','402       ',
     &'401       ','413       ','402       ','401       ',
     &'413       ','402       ','401       ','403       ',
     &'413       ','403       ','402       ','401       ',
     &'403       ','404       ','413       ','404       ',
     &'402       ','401       ','403       ','404       ',
     &'413       ','402       ','402       ','401       ',
     &'403       ','413       ','401       ','401       ',
     &'402       ','401       ','403       ','404       ',
     &'413       ','402       ','401       ','403       ',
     &'404       ','413       ','402       ','401       ',
     &'403       ','413       ','402       ','402       ',
     &'401       ','403       ','413       ','403       ',
     &'413       ','402       ','402       ','402       ',
     &'401       ','403       ','404       ','403       '/
      DATA (PVREF(I),I= 181, 240)/
     &'413       ','406       ','406       ','406       ',
     &'402       ','402       ','402       ','402       ',
     &'402       ','402       ','401       ','403       ',
     &'413       ','403       ','413       ','404       ',
     &'413       ','406       ','402       ','401       ',
     &'413       ','402       ','401       ','413       ',
     &'402       ','401       ','413       ','401       ',
     &'401       ','401       ','401       ','401       ',
     &'401       ','401       ','401       ','401       ',
     &'401       ','403       ','404       ','403       ',
     &'404       ','413       ','403       ','403       ',
     &'404       ','413       ','403       ','403       ',
     &'404       ','413       ','404       ','404       ',
     &'404       ','404       ','404       ','404       ',
     &'404       ','401       ','401       ','401       '/
      DATA (PVREF(I),I= 241, 300)/
     &'401       ','401       ','402       ','402       ',
     &'402       ','402       ','402       ','402       ',
     &'402       ','401       ','403       ','404       ',
     &'403       ','413       ','403       ','404       ',
     &'413       ','403       ','413       ','403       ',
     &'403       ','404       ','403       ','413       ',
     &'401       ','401       ','401       ','401       ',
     &'402       ','401       ','403       ','413       ',
     &'402       ','401       ','413       ','401       ',
     &'401       ','403       ','401       ','403       ',
     &'404       ','413       ','402       ','402       ',
     &'401       ','403       ','404       ','413       ',
     &'402       ','401       ','403       ','413       ',
     &'402       ','401       ','413       ','401       ',
     &'402       ','401       ','413       ','402       '/
      DATA (PVREF(I),I= 301, 360)/
     &'401       ','413       ','402       ','401       ',
     &'413       ','402       ','401       ','413       ',
     &'401       ','402       ','401       ','413       ',
     &'402       ','401       ','413       ','402       ',
     &'401       ','413       ','401       ','402       ',
     &'401       ','413       ','402       ','401       ',
     &'413       ','402       ','401       ','413       ',
     &'401       ','402       ','402       ','402       ',
     &'401       ','413       ','402       ','402       ',
     &'401       ','403       ','404       ','413       ',
     &'404       ','402       ','401       ','413       ',
     &'402       ','401       ','413       ','402       ',
     &'403       ','403       ','401       ','403       ',
     &'413       ','402       ','401       ','413       ',
     &'402       ','401       ','403       ','413       '/
      DATA (PVREF(I),I= 361, 420)/
     &'402       ','401       ','413       ','401       ',
     &'402       ','401       ','403       ','404       ',
     &'413       ','402       ','402       ','402       ',
     &'401       ','403       ','413       ','402       ',
     &'402       ','402       ','402       ','402       ',
     &'402       ','401       ','413       ','402       ',
     &'402       ','402       ','402       ','402       ',
     &'403       ','404       ','413       ','403       ',
     &'404       ','413       ','403       ','403       ',
     &'404       ','413       ','403       ','403       ',
     &'403       ','404       ','413       ','401       ',
     &'404       ','413       ','404       ','413       ',
     &'402       ','401       ','413       ','401       ',
     &'402       ','402       ','402       ','401       ',
     &'403       ','404       ','413       ','403       '/
      DATA (PVREF(I),I= 421, 480)/
     &'404       ','413       ','403       ','403       ',
     &'403       ','404       ','402       ','401       ',
     &'413       ','402       ','401       ','413       ',
     &'401       ','401       ','402       ','402       ',
     &'402       ','402       ','402       ','402       ',
     &'402       ','402       ','402       ','402       ',
     &'401       ','402       ','402       ','402       ',
     &'402       ','402       ','402       ','402       ',
     &'402       ','401       ','403       ','401       ',
     &'403       ','403       ','403       ','413       ',
     &'403       ','402       ','401       ','403       ',
     &'413       ','402       ','401       ','403       ',
     &'413       ','402       ','403       ','401       ',
     &'413       ','410       ','409       ','409       ',
     &'409       ','409       ','409       ','409       '/
      DATA (PVREF(I),I= 481, 540)/
     &'409       ','409       ','409       ','410       ',
     &'409       ','409       ','409       ','409       ',
     &'409       ','409       ','402       ','410       ',
     &'409       ','409       ','409       ','409       ',
     &'409       ','409       ','409       ','409       ',
     &'409       ','409       ','402       ','410       ',
     &'410       ','410       ','403       ','404       ',
     &'404       ','403       ','403       ','404       ',
     &'413       ','404       ','404       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'409       ','409       ','409       ','409       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       '/
      DATA (PVREF(I),I= 541, 600)/
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'405       ','405       ','405       ','405       ',
     &'406       ','407       ','412       ','407       ',
     &'407       ','407       ','407       ','407       ',
     &'407       ','412       ','407       ','407       '/
      DATA (PVREF(I),I= 601, 660)/
     &'407       ','407       ','406       ','407       ',
     &'412       ','407       ','407       ','406       ',
     &'407       ','412       ','407       ','407       ',
     &'407       ','412       ','407       ','407       ',
     &'412       ','407       ','412       ','406       ',
     &'409       ','407       ','412       ','407       ',
     &'412       ','407       ','412       ','407       ',
     &'406       ','407       ','406       ','407       ',
     &'412       ','406       ','407       ','412       ',
     &'406       ','407       ','412       ','406       ',
     &'407       ','412       ','406       ','407       ',
     &'412       ','406       ','407       ','407       ',
     &'407       ','406       ','407       ','412       ',
     &'406       ','407       ','412       ','406       ',
     &'407       ','412       ','406       ','407       '/
      DATA (PVREF(I),I= 661, 720)/
     &'406       ','407       ','412       ','406       ',
     &'406       ','407       ','412       ','407       ',
     &'412       ','407       ','407       ','406       ',
     &'407       ','406       ','407       ','406       ',
     &'407       ','412       ','406       ','407       ',
     &'412       ','406       ','406       ','406       ',
     &'407       ','412       ','406       ','406       ',
     &'407       ','412       ','406       ','407       ',
     &'407       ','407       ','407       ','406       ',
     &'407       ','412       ','406       ','406       ',
     &'406       ','407       ','412       ','406       ',
     &'406       ','407       ','412       ','407       ',
     &'406       ','407       ','406       ','407       ',
     &'412       ','409       ','409       ','406       ',
     &'407       ','412       ','407       ','407       '/
      DATA (PVREF(I),I= 721, 780)/
     &'407       ','412       ','407       ','406       ',
     &'412       ','406       ','407       ','412       ',
     &'406       ','407       ','412       ','407       ',
     &'406       ','407       ','412       ','406       ',
     &'410       ','410       ','407       ','412       ',
     &'406       ','407       ','412       ','407       ',
     &'407       ','406       ','407       ','412       ',
     &'406       ','406       ','407       ','412       ',
     &'407       ','407       ','406       ','412       ',
     &'407       ','406       ','407       ','412       ',
     &'407       ','406       ','408       ','408       ',
     &'408       ','408       ','408       ','408       ',
     &'408       ','408       ','408       ','408       ',
     &'408       ','409       ','410       ','408       ',
     &'408       ','408       ','408       ','408       '/
      DATA (PVREF(I),I= 781, NCODES)/
     &'408       ','409       ','408       ','409       ',
     &'408       ','409       ','408       ','408       ',
     &'409       ','408       ','408       ','408       ',
     &'408       ','408       ','408       ','408       ',
     &'408       ','408       ','408       ','408       ',
     &'408       ','408       ','408       ','408       ',
     &'408       ','408       ','408       ','409       ',
     &'408       ','410       ','408       ','410       ',
     &'409       ','409       ','409       ','410       ',
     &'409       ','409       ','409       ','410       ',
     &'409       ','410       ','410       ','410       ',
     &'409       ','410       ','409       '/
C----------
C  MAP PV/REFERENCE CODES INTO A FVS HABITAT/ECOCLASS CODE
C----------
      KODTYP=0
      DO I=1,NCODES
      IF((ADJUSTL(PVCODE(I)).EQ.ADJUSTL(KARD2)).AND.(ADJUSTL(PVREF(I))
     &   .EQ.ADJUSTL(CPVREF)))THEN
        READ(HABPVR(I),'(I4)') KODTYP
        LPVCOD=.TRUE.
        LPVREF=.TRUE.
        EXIT
      ENDIF
      IF(ADJUSTL(PVCODE(I)).EQ.ADJUSTL(KARD2))LPVCOD=.TRUE.
      IF(ADJUSTL(PVREF(I)).EQ.ADJUSTL(CPVREF))LPVREF=.TRUE.
      ENDDO
C
      RETURN
      END