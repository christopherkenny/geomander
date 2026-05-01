# Filter Adjacency to the Edges Along a Seam

Keep only the adjacency edges that connect the two sides of a selected
seam.

## Usage

``` r
seam_adj(adj, shp, admin, seam, epsg = 3857)
```

## Arguments

- adj:

  Zero-indexed adjacency graph.

- shp:

  `sf` object containing the administrative identifier.

- admin:

  Name of the administrative-unit column in `shp`.

- seam:

  Length-2 vector of administrative-unit values defining the seam.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

adjacency list containing only cross-seam edges

## Examples

``` r
data('rockland')
data('orange')
data('nrcsd')

o_and_r <- rbind(orange, rockland)
o_and_r <- o_and_r |>
  geo_filter(nrcsd) |>
  geo_trim(nrcsd)
adj <- adjacency(o_and_r)

seam_adj(adj, shp = o_and_r, admin = 'county', seam = c('071', '087'))
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> integer(0)
#> 
#> [[3]]
#> integer(0)
#> 
#> [[4]]
#> integer(0)
#> 
#> [[5]]
#> integer(0)
#> 
#> [[6]]
#> integer(0)
#> 
#> [[7]]
#> [1] 659 747
#> 
#> [[8]]
#> integer(0)
#> 
#> [[9]]
#> integer(0)
#> 
#> [[10]]
#> integer(0)
#> 
#> [[11]]
#> integer(0)
#> 
#> [[12]]
#> integer(0)
#> 
#> [[13]]
#> integer(0)
#> 
#> [[14]]
#> integer(0)
#> 
#> [[15]]
#> integer(0)
#> 
#> [[16]]
#> integer(0)
#> 
#> [[17]]
#> [1] 659
#> 
#> [[18]]
#> [1] 157
#> 
#> [[19]]
#> integer(0)
#> 
#> [[20]]
#> integer(0)
#> 
#> [[21]]
#> integer(0)
#> 
#> [[22]]
#> integer(0)
#> 
#> [[23]]
#> [1] 366
#> 
#> [[24]]
#> integer(0)
#> 
#> [[25]]
#> integer(0)
#> 
#> [[26]]
#> integer(0)
#> 
#> [[27]]
#> integer(0)
#> 
#> [[28]]
#> integer(0)
#> 
#> [[29]]
#> integer(0)
#> 
#> [[30]]
#> integer(0)
#> 
#> [[31]]
#> integer(0)
#> 
#> [[32]]
#> integer(0)
#> 
#> [[33]]
#> integer(0)
#> 
#> [[34]]
#> integer(0)
#> 
#> [[35]]
#> [1] 659 157 171
#> 
#> [[36]]
#> integer(0)
#> 
#> [[37]]
#> integer(0)
#> 
#> [[38]]
#> integer(0)
#> 
#> [[39]]
#> integer(0)
#> 
#> [[40]]
#> integer(0)
#> 
#> [[41]]
#> integer(0)
#> 
#> [[42]]
#> integer(0)
#> 
#> [[43]]
#> [1] 164
#> 
#> [[44]]
#> [1] 164
#> 
#> [[45]]
#> integer(0)
#> 
#> [[46]]
#> integer(0)
#> 
#> [[47]]
#> integer(0)
#> 
#> [[48]]
#> integer(0)
#> 
#> [[49]]
#> integer(0)
#> 
#> [[50]]
#> [1] 121
#> 
#> [[51]]
#> integer(0)
#> 
#> [[52]]
#> [1] 382
#> 
#> [[53]]
#> integer(0)
#> 
#> [[54]]
#> integer(0)
#> 
#> [[55]]
#> integer(0)
#> 
#> [[56]]
#> integer(0)
#> 
#> [[57]]
#> integer(0)
#> 
#> [[58]]
#> [1] 382
#> 
#> [[59]]
#> integer(0)
#> 
#> [[60]]
#> integer(0)
#> 
#> [[61]]
#> integer(0)
#> 
#> [[62]]
#> integer(0)
#> 
#> [[63]]
#> integer(0)
#> 
#> [[64]]
#> integer(0)
#> 
#> [[65]]
#> [1] 155
#> 
#> [[66]]
#> integer(0)
#> 
#> [[67]]
#> integer(0)
#> 
#> [[68]]
#> integer(0)
#> 
#> [[69]]
#> integer(0)
#> 
#> [[70]]
#> integer(0)
#> 
#> [[71]]
#> integer(0)
#> 
#> [[72]]
#> integer(0)
#> 
#> [[73]]
#> [1] 100
#> 
#> [[74]]
#> integer(0)
#> 
#> [[75]]
#> integer(0)
#> 
#> [[76]]
#> integer(0)
#> 
#> [[77]]
#> integer(0)
#> 
#> [[78]]
#> integer(0)
#> 
#> [[79]]
#> integer(0)
#> 
#> [[80]]
#> integer(0)
#> 
#> [[81]]
#> integer(0)
#> 
#> [[82]]
#> integer(0)
#> 
#> [[83]]
#> integer(0)
#> 
#> [[84]]
#> integer(0)
#> 
#> [[85]]
#> integer(0)
#> 
#> [[86]]
#> [1] 778
#> 
#> [[87]]
#> integer(0)
#> 
#> [[88]]
#> integer(0)
#> 
#> [[89]]
#> integer(0)
#> 
#> [[90]]
#> integer(0)
#> 
#> [[91]]
#> integer(0)
#> 
#> [[92]]
#> integer(0)
#> 
#> [[93]]
#> integer(0)
#> 
#> [[94]]
#> integer(0)
#> 
#> [[95]]
#> integer(0)
#> 
#> [[96]]
#> integer(0)
#> 
#> [[97]]
#> integer(0)
#> 
#> [[98]]
#> integer(0)
#> 
#> [[99]]
#> integer(0)
#> 
#> [[100]]
#> integer(0)
#> 
#> [[101]]
#> [1] 72
#> 
#> [[102]]
#> integer(0)
#> 
#> [[103]]
#> integer(0)
#> 
#> [[104]]
#> integer(0)
#> 
#> [[105]]
#> integer(0)
#> 
#> [[106]]
#> integer(0)
#> 
#> [[107]]
#> integer(0)
#> 
#> [[108]]
#> integer(0)
#> 
#> [[109]]
#> integer(0)
#> 
#> [[110]]
#> integer(0)
#> 
#> [[111]]
#> integer(0)
#> 
#> [[112]]
#> integer(0)
#> 
#> [[113]]
#> integer(0)
#> 
#> [[114]]
#> integer(0)
#> 
#> [[115]]
#> integer(0)
#> 
#> [[116]]
#> integer(0)
#> 
#> [[117]]
#> integer(0)
#> 
#> [[118]]
#> integer(0)
#> 
#> [[119]]
#> integer(0)
#> 
#> [[120]]
#> integer(0)
#> 
#> [[121]]
#> integer(0)
#> 
#> [[122]]
#> [1] 49
#> 
#> [[123]]
#> integer(0)
#> 
#> [[124]]
#> integer(0)
#> 
#> [[125]]
#> integer(0)
#> 
#> [[126]]
#> integer(0)
#> 
#> [[127]]
#> integer(0)
#> 
#> [[128]]
#> integer(0)
#> 
#> [[129]]
#> integer(0)
#> 
#> [[130]]
#> integer(0)
#> 
#> [[131]]
#> integer(0)
#> 
#> [[132]]
#> integer(0)
#> 
#> [[133]]
#> integer(0)
#> 
#> [[134]]
#> integer(0)
#> 
#> [[135]]
#> integer(0)
#> 
#> [[136]]
#> integer(0)
#> 
#> [[137]]
#> integer(0)
#> 
#> [[138]]
#> integer(0)
#> 
#> [[139]]
#> integer(0)
#> 
#> [[140]]
#> integer(0)
#> 
#> [[141]]
#> integer(0)
#> 
#> [[142]]
#> integer(0)
#> 
#> [[143]]
#> integer(0)
#> 
#> [[144]]
#> integer(0)
#> 
#> [[145]]
#> integer(0)
#> 
#> [[146]]
#> integer(0)
#> 
#> [[147]]
#> integer(0)
#> 
#> [[148]]
#> integer(0)
#> 
#> [[149]]
#> integer(0)
#> 
#> [[150]]
#> integer(0)
#> 
#> [[151]]
#> integer(0)
#> 
#> [[152]]
#> integer(0)
#> 
#> [[153]]
#> integer(0)
#> 
#> [[154]]
#> integer(0)
#> 
#> [[155]]
#> integer(0)
#> 
#> [[156]]
#> [1] 64
#> 
#> [[157]]
#> integer(0)
#> 
#> [[158]]
#> [1] 17 34
#> 
#> [[159]]
#> integer(0)
#> 
#> [[160]]
#> integer(0)
#> 
#> [[161]]
#> integer(0)
#> 
#> [[162]]
#> integer(0)
#> 
#> [[163]]
#> integer(0)
#> 
#> [[164]]
#> integer(0)
#> 
#> [[165]]
#> [1] 43 42
#> 
#> [[166]]
#> integer(0)
#> 
#> [[167]]
#> integer(0)
#> 
#> [[168]]
#> integer(0)
#> 
#> [[169]]
#> integer(0)
#> 
#> [[170]]
#> integer(0)
#> 
#> [[171]]
#> integer(0)
#> 
#> [[172]]
#> [1] 34
#> 
#> [[173]]
#> integer(0)
#> 
#> [[174]]
#> integer(0)
#> 
#> [[175]]
#> integer(0)
#> 
#> [[176]]
#> integer(0)
#> 
#> [[177]]
#> integer(0)
#> 
#> [[178]]
#> integer(0)
#> 
#> [[179]]
#> integer(0)
#> 
#> [[180]]
#> integer(0)
#> 
#> [[181]]
#> integer(0)
#> 
#> [[182]]
#> integer(0)
#> 
#> [[183]]
#> integer(0)
#> 
#> [[184]]
#> integer(0)
#> 
#> [[185]]
#> integer(0)
#> 
#> [[186]]
#> integer(0)
#> 
#> [[187]]
#> integer(0)
#> 
#> [[188]]
#> integer(0)
#> 
#> [[189]]
#> integer(0)
#> 
#> [[190]]
#> integer(0)
#> 
#> [[191]]
#> integer(0)
#> 
#> [[192]]
#> integer(0)
#> 
#> [[193]]
#> integer(0)
#> 
#> [[194]]
#> integer(0)
#> 
#> [[195]]
#> integer(0)
#> 
#> [[196]]
#> integer(0)
#> 
#> [[197]]
#> integer(0)
#> 
#> [[198]]
#> integer(0)
#> 
#> [[199]]
#> integer(0)
#> 
#> [[200]]
#> integer(0)
#> 
#> [[201]]
#> integer(0)
#> 
#> [[202]]
#> integer(0)
#> 
#> [[203]]
#> integer(0)
#> 
#> [[204]]
#> integer(0)
#> 
#> [[205]]
#> integer(0)
#> 
#> [[206]]
#> integer(0)
#> 
#> [[207]]
#> integer(0)
#> 
#> [[208]]
#> integer(0)
#> 
#> [[209]]
#> integer(0)
#> 
#> [[210]]
#> integer(0)
#> 
#> [[211]]
#> integer(0)
#> 
#> [[212]]
#> integer(0)
#> 
#> [[213]]
#> integer(0)
#> 
#> [[214]]
#> integer(0)
#> 
#> [[215]]
#> integer(0)
#> 
#> [[216]]
#> integer(0)
#> 
#> [[217]]
#> integer(0)
#> 
#> [[218]]
#> integer(0)
#> 
#> [[219]]
#> integer(0)
#> 
#> [[220]]
#> integer(0)
#> 
#> [[221]]
#> integer(0)
#> 
#> [[222]]
#> integer(0)
#> 
#> [[223]]
#> integer(0)
#> 
#> [[224]]
#> integer(0)
#> 
#> [[225]]
#> integer(0)
#> 
#> [[226]]
#> integer(0)
#> 
#> [[227]]
#> integer(0)
#> 
#> [[228]]
#> integer(0)
#> 
#> [[229]]
#> integer(0)
#> 
#> [[230]]
#> integer(0)
#> 
#> [[231]]
#> integer(0)
#> 
#> [[232]]
#> integer(0)
#> 
#> [[233]]
#> integer(0)
#> 
#> [[234]]
#> integer(0)
#> 
#> [[235]]
#> integer(0)
#> 
#> [[236]]
#> integer(0)
#> 
#> [[237]]
#> integer(0)
#> 
#> [[238]]
#> integer(0)
#> 
#> [[239]]
#> integer(0)
#> 
#> [[240]]
#> integer(0)
#> 
#> [[241]]
#> integer(0)
#> 
#> [[242]]
#> integer(0)
#> 
#> [[243]]
#> integer(0)
#> 
#> [[244]]
#> integer(0)
#> 
#> [[245]]
#> integer(0)
#> 
#> [[246]]
#> integer(0)
#> 
#> [[247]]
#> integer(0)
#> 
#> [[248]]
#> integer(0)
#> 
#> [[249]]
#> integer(0)
#> 
#> [[250]]
#> integer(0)
#> 
#> [[251]]
#> integer(0)
#> 
#> [[252]]
#> integer(0)
#> 
#> [[253]]
#> integer(0)
#> 
#> [[254]]
#> integer(0)
#> 
#> [[255]]
#> integer(0)
#> 
#> [[256]]
#> integer(0)
#> 
#> [[257]]
#> integer(0)
#> 
#> [[258]]
#> integer(0)
#> 
#> [[259]]
#> integer(0)
#> 
#> [[260]]
#> integer(0)
#> 
#> [[261]]
#> integer(0)
#> 
#> [[262]]
#> integer(0)
#> 
#> [[263]]
#> integer(0)
#> 
#> [[264]]
#> integer(0)
#> 
#> [[265]]
#> integer(0)
#> 
#> [[266]]
#> integer(0)
#> 
#> [[267]]
#> integer(0)
#> 
#> [[268]]
#> integer(0)
#> 
#> [[269]]
#> integer(0)
#> 
#> [[270]]
#> integer(0)
#> 
#> [[271]]
#> integer(0)
#> 
#> [[272]]
#> integer(0)
#> 
#> [[273]]
#> integer(0)
#> 
#> [[274]]
#> integer(0)
#> 
#> [[275]]
#> integer(0)
#> 
#> [[276]]
#> integer(0)
#> 
#> [[277]]
#> integer(0)
#> 
#> [[278]]
#> integer(0)
#> 
#> [[279]]
#> integer(0)
#> 
#> [[280]]
#> integer(0)
#> 
#> [[281]]
#> integer(0)
#> 
#> [[282]]
#> integer(0)
#> 
#> [[283]]
#> integer(0)
#> 
#> [[284]]
#> integer(0)
#> 
#> [[285]]
#> integer(0)
#> 
#> [[286]]
#> integer(0)
#> 
#> [[287]]
#> integer(0)
#> 
#> [[288]]
#> integer(0)
#> 
#> [[289]]
#> integer(0)
#> 
#> [[290]]
#> integer(0)
#> 
#> [[291]]
#> integer(0)
#> 
#> [[292]]
#> integer(0)
#> 
#> [[293]]
#> integer(0)
#> 
#> [[294]]
#> integer(0)
#> 
#> [[295]]
#> integer(0)
#> 
#> [[296]]
#> integer(0)
#> 
#> [[297]]
#> integer(0)
#> 
#> [[298]]
#> integer(0)
#> 
#> [[299]]
#> integer(0)
#> 
#> [[300]]
#> integer(0)
#> 
#> [[301]]
#> integer(0)
#> 
#> [[302]]
#> integer(0)
#> 
#> [[303]]
#> integer(0)
#> 
#> [[304]]
#> integer(0)
#> 
#> [[305]]
#> integer(0)
#> 
#> [[306]]
#> integer(0)
#> 
#> [[307]]
#> integer(0)
#> 
#> [[308]]
#> integer(0)
#> 
#> [[309]]
#> integer(0)
#> 
#> [[310]]
#> integer(0)
#> 
#> [[311]]
#> integer(0)
#> 
#> [[312]]
#> integer(0)
#> 
#> [[313]]
#> integer(0)
#> 
#> [[314]]
#> integer(0)
#> 
#> [[315]]
#> integer(0)
#> 
#> [[316]]
#> integer(0)
#> 
#> [[317]]
#> integer(0)
#> 
#> [[318]]
#> integer(0)
#> 
#> [[319]]
#> integer(0)
#> 
#> [[320]]
#> integer(0)
#> 
#> [[321]]
#> integer(0)
#> 
#> [[322]]
#> integer(0)
#> 
#> [[323]]
#> integer(0)
#> 
#> [[324]]
#> integer(0)
#> 
#> [[325]]
#> integer(0)
#> 
#> [[326]]
#> integer(0)
#> 
#> [[327]]
#> integer(0)
#> 
#> [[328]]
#> integer(0)
#> 
#> [[329]]
#> integer(0)
#> 
#> [[330]]
#> integer(0)
#> 
#> [[331]]
#> integer(0)
#> 
#> [[332]]
#> integer(0)
#> 
#> [[333]]
#> integer(0)
#> 
#> [[334]]
#> integer(0)
#> 
#> [[335]]
#> integer(0)
#> 
#> [[336]]
#> integer(0)
#> 
#> [[337]]
#> integer(0)
#> 
#> [[338]]
#> integer(0)
#> 
#> [[339]]
#> integer(0)
#> 
#> [[340]]
#> integer(0)
#> 
#> [[341]]
#> integer(0)
#> 
#> [[342]]
#> integer(0)
#> 
#> [[343]]
#> integer(0)
#> 
#> [[344]]
#> integer(0)
#> 
#> [[345]]
#> integer(0)
#> 
#> [[346]]
#> integer(0)
#> 
#> [[347]]
#> integer(0)
#> 
#> [[348]]
#> integer(0)
#> 
#> [[349]]
#> integer(0)
#> 
#> [[350]]
#> integer(0)
#> 
#> [[351]]
#> integer(0)
#> 
#> [[352]]
#> integer(0)
#> 
#> [[353]]
#> integer(0)
#> 
#> [[354]]
#> integer(0)
#> 
#> [[355]]
#> integer(0)
#> 
#> [[356]]
#> integer(0)
#> 
#> [[357]]
#> integer(0)
#> 
#> [[358]]
#> integer(0)
#> 
#> [[359]]
#> integer(0)
#> 
#> [[360]]
#> integer(0)
#> 
#> [[361]]
#> integer(0)
#> 
#> [[362]]
#> integer(0)
#> 
#> [[363]]
#> integer(0)
#> 
#> [[364]]
#> integer(0)
#> 
#> [[365]]
#> integer(0)
#> 
#> [[366]]
#> integer(0)
#> 
#> [[367]]
#> [1] 22
#> 
#> [[368]]
#> integer(0)
#> 
#> [[369]]
#> integer(0)
#> 
#> [[370]]
#> integer(0)
#> 
#> [[371]]
#> integer(0)
#> 
#> [[372]]
#> integer(0)
#> 
#> [[373]]
#> integer(0)
#> 
#> [[374]]
#> integer(0)
#> 
#> [[375]]
#> integer(0)
#> 
#> [[376]]
#> integer(0)
#> 
#> [[377]]
#> integer(0)
#> 
#> [[378]]
#> integer(0)
#> 
#> [[379]]
#> integer(0)
#> 
#> [[380]]
#> integer(0)
#> 
#> [[381]]
#> integer(0)
#> 
#> [[382]]
#> integer(0)
#> 
#> [[383]]
#> [1] 51 57
#> 
#> [[384]]
#> integer(0)
#> 
#> [[385]]
#> integer(0)
#> 
#> [[386]]
#> integer(0)
#> 
#> [[387]]
#> integer(0)
#> 
#> [[388]]
#> integer(0)
#> 
#> [[389]]
#> integer(0)
#> 
#> [[390]]
#> integer(0)
#> 
#> [[391]]
#> integer(0)
#> 
#> [[392]]
#> integer(0)
#> 
#> [[393]]
#> integer(0)
#> 
#> [[394]]
#> integer(0)
#> 
#> [[395]]
#> integer(0)
#> 
#> [[396]]
#> integer(0)
#> 
#> [[397]]
#> integer(0)
#> 
#> [[398]]
#> integer(0)
#> 
#> [[399]]
#> integer(0)
#> 
#> [[400]]
#> integer(0)
#> 
#> [[401]]
#> integer(0)
#> 
#> [[402]]
#> integer(0)
#> 
#> [[403]]
#> integer(0)
#> 
#> [[404]]
#> integer(0)
#> 
#> [[405]]
#> integer(0)
#> 
#> [[406]]
#> integer(0)
#> 
#> [[407]]
#> integer(0)
#> 
#> [[408]]
#> integer(0)
#> 
#> [[409]]
#> integer(0)
#> 
#> [[410]]
#> integer(0)
#> 
#> [[411]]
#> integer(0)
#> 
#> [[412]]
#> integer(0)
#> 
#> [[413]]
#> integer(0)
#> 
#> [[414]]
#> integer(0)
#> 
#> [[415]]
#> integer(0)
#> 
#> [[416]]
#> integer(0)
#> 
#> [[417]]
#> integer(0)
#> 
#> [[418]]
#> integer(0)
#> 
#> [[419]]
#> integer(0)
#> 
#> [[420]]
#> integer(0)
#> 
#> [[421]]
#> integer(0)
#> 
#> [[422]]
#> integer(0)
#> 
#> [[423]]
#> integer(0)
#> 
#> [[424]]
#> integer(0)
#> 
#> [[425]]
#> integer(0)
#> 
#> [[426]]
#> integer(0)
#> 
#> [[427]]
#> integer(0)
#> 
#> [[428]]
#> integer(0)
#> 
#> [[429]]
#> integer(0)
#> 
#> [[430]]
#> integer(0)
#> 
#> [[431]]
#> integer(0)
#> 
#> [[432]]
#> integer(0)
#> 
#> [[433]]
#> integer(0)
#> 
#> [[434]]
#> integer(0)
#> 
#> [[435]]
#> integer(0)
#> 
#> [[436]]
#> integer(0)
#> 
#> [[437]]
#> integer(0)
#> 
#> [[438]]
#> integer(0)
#> 
#> [[439]]
#> integer(0)
#> 
#> [[440]]
#> integer(0)
#> 
#> [[441]]
#> integer(0)
#> 
#> [[442]]
#> integer(0)
#> 
#> [[443]]
#> integer(0)
#> 
#> [[444]]
#> integer(0)
#> 
#> [[445]]
#> integer(0)
#> 
#> [[446]]
#> integer(0)
#> 
#> [[447]]
#> integer(0)
#> 
#> [[448]]
#> integer(0)
#> 
#> [[449]]
#> integer(0)
#> 
#> [[450]]
#> integer(0)
#> 
#> [[451]]
#> integer(0)
#> 
#> [[452]]
#> integer(0)
#> 
#> [[453]]
#> integer(0)
#> 
#> [[454]]
#> integer(0)
#> 
#> [[455]]
#> integer(0)
#> 
#> [[456]]
#> integer(0)
#> 
#> [[457]]
#> integer(0)
#> 
#> [[458]]
#> integer(0)
#> 
#> [[459]]
#> integer(0)
#> 
#> [[460]]
#> integer(0)
#> 
#> [[461]]
#> integer(0)
#> 
#> [[462]]
#> integer(0)
#> 
#> [[463]]
#> integer(0)
#> 
#> [[464]]
#> integer(0)
#> 
#> [[465]]
#> integer(0)
#> 
#> [[466]]
#> integer(0)
#> 
#> [[467]]
#> integer(0)
#> 
#> [[468]]
#> integer(0)
#> 
#> [[469]]
#> integer(0)
#> 
#> [[470]]
#> integer(0)
#> 
#> [[471]]
#> integer(0)
#> 
#> [[472]]
#> integer(0)
#> 
#> [[473]]
#> integer(0)
#> 
#> [[474]]
#> integer(0)
#> 
#> [[475]]
#> integer(0)
#> 
#> [[476]]
#> integer(0)
#> 
#> [[477]]
#> integer(0)
#> 
#> [[478]]
#> integer(0)
#> 
#> [[479]]
#> integer(0)
#> 
#> [[480]]
#> integer(0)
#> 
#> [[481]]
#> integer(0)
#> 
#> [[482]]
#> integer(0)
#> 
#> [[483]]
#> integer(0)
#> 
#> [[484]]
#> integer(0)
#> 
#> [[485]]
#> integer(0)
#> 
#> [[486]]
#> integer(0)
#> 
#> [[487]]
#> integer(0)
#> 
#> [[488]]
#> integer(0)
#> 
#> [[489]]
#> integer(0)
#> 
#> [[490]]
#> integer(0)
#> 
#> [[491]]
#> integer(0)
#> 
#> [[492]]
#> integer(0)
#> 
#> [[493]]
#> integer(0)
#> 
#> [[494]]
#> integer(0)
#> 
#> [[495]]
#> integer(0)
#> 
#> [[496]]
#> integer(0)
#> 
#> [[497]]
#> integer(0)
#> 
#> [[498]]
#> integer(0)
#> 
#> [[499]]
#> integer(0)
#> 
#> [[500]]
#> integer(0)
#> 
#> [[501]]
#> integer(0)
#> 
#> [[502]]
#> integer(0)
#> 
#> [[503]]
#> integer(0)
#> 
#> [[504]]
#> integer(0)
#> 
#> [[505]]
#> integer(0)
#> 
#> [[506]]
#> integer(0)
#> 
#> [[507]]
#> integer(0)
#> 
#> [[508]]
#> integer(0)
#> 
#> [[509]]
#> integer(0)
#> 
#> [[510]]
#> integer(0)
#> 
#> [[511]]
#> integer(0)
#> 
#> [[512]]
#> integer(0)
#> 
#> [[513]]
#> integer(0)
#> 
#> [[514]]
#> integer(0)
#> 
#> [[515]]
#> integer(0)
#> 
#> [[516]]
#> integer(0)
#> 
#> [[517]]
#> integer(0)
#> 
#> [[518]]
#> integer(0)
#> 
#> [[519]]
#> integer(0)
#> 
#> [[520]]
#> integer(0)
#> 
#> [[521]]
#> integer(0)
#> 
#> [[522]]
#> integer(0)
#> 
#> [[523]]
#> integer(0)
#> 
#> [[524]]
#> integer(0)
#> 
#> [[525]]
#> integer(0)
#> 
#> [[526]]
#> integer(0)
#> 
#> [[527]]
#> integer(0)
#> 
#> [[528]]
#> integer(0)
#> 
#> [[529]]
#> integer(0)
#> 
#> [[530]]
#> integer(0)
#> 
#> [[531]]
#> integer(0)
#> 
#> [[532]]
#> integer(0)
#> 
#> [[533]]
#> integer(0)
#> 
#> [[534]]
#> integer(0)
#> 
#> [[535]]
#> integer(0)
#> 
#> [[536]]
#> integer(0)
#> 
#> [[537]]
#> integer(0)
#> 
#> [[538]]
#> integer(0)
#> 
#> [[539]]
#> integer(0)
#> 
#> [[540]]
#> integer(0)
#> 
#> [[541]]
#> integer(0)
#> 
#> [[542]]
#> integer(0)
#> 
#> [[543]]
#> integer(0)
#> 
#> [[544]]
#> integer(0)
#> 
#> [[545]]
#> integer(0)
#> 
#> [[546]]
#> integer(0)
#> 
#> [[547]]
#> integer(0)
#> 
#> [[548]]
#> integer(0)
#> 
#> [[549]]
#> integer(0)
#> 
#> [[550]]
#> integer(0)
#> 
#> [[551]]
#> integer(0)
#> 
#> [[552]]
#> integer(0)
#> 
#> [[553]]
#> integer(0)
#> 
#> [[554]]
#> integer(0)
#> 
#> [[555]]
#> integer(0)
#> 
#> [[556]]
#> integer(0)
#> 
#> [[557]]
#> integer(0)
#> 
#> [[558]]
#> integer(0)
#> 
#> [[559]]
#> integer(0)
#> 
#> [[560]]
#> integer(0)
#> 
#> [[561]]
#> integer(0)
#> 
#> [[562]]
#> integer(0)
#> 
#> [[563]]
#> integer(0)
#> 
#> [[564]]
#> integer(0)
#> 
#> [[565]]
#> integer(0)
#> 
#> [[566]]
#> integer(0)
#> 
#> [[567]]
#> integer(0)
#> 
#> [[568]]
#> integer(0)
#> 
#> [[569]]
#> integer(0)
#> 
#> [[570]]
#> integer(0)
#> 
#> [[571]]
#> integer(0)
#> 
#> [[572]]
#> integer(0)
#> 
#> [[573]]
#> integer(0)
#> 
#> [[574]]
#> integer(0)
#> 
#> [[575]]
#> integer(0)
#> 
#> [[576]]
#> integer(0)
#> 
#> [[577]]
#> integer(0)
#> 
#> [[578]]
#> integer(0)
#> 
#> [[579]]
#> integer(0)
#> 
#> [[580]]
#> integer(0)
#> 
#> [[581]]
#> integer(0)
#> 
#> [[582]]
#> integer(0)
#> 
#> [[583]]
#> integer(0)
#> 
#> [[584]]
#> integer(0)
#> 
#> [[585]]
#> integer(0)
#> 
#> [[586]]
#> integer(0)
#> 
#> [[587]]
#> integer(0)
#> 
#> [[588]]
#> integer(0)
#> 
#> [[589]]
#> integer(0)
#> 
#> [[590]]
#> integer(0)
#> 
#> [[591]]
#> integer(0)
#> 
#> [[592]]
#> integer(0)
#> 
#> [[593]]
#> integer(0)
#> 
#> [[594]]
#> integer(0)
#> 
#> [[595]]
#> integer(0)
#> 
#> [[596]]
#> integer(0)
#> 
#> [[597]]
#> integer(0)
#> 
#> [[598]]
#> integer(0)
#> 
#> [[599]]
#> integer(0)
#> 
#> [[600]]
#> integer(0)
#> 
#> [[601]]
#> integer(0)
#> 
#> [[602]]
#> integer(0)
#> 
#> [[603]]
#> integer(0)
#> 
#> [[604]]
#> integer(0)
#> 
#> [[605]]
#> integer(0)
#> 
#> [[606]]
#> integer(0)
#> 
#> [[607]]
#> integer(0)
#> 
#> [[608]]
#> integer(0)
#> 
#> [[609]]
#> integer(0)
#> 
#> [[610]]
#> integer(0)
#> 
#> [[611]]
#> integer(0)
#> 
#> [[612]]
#> integer(0)
#> 
#> [[613]]
#> integer(0)
#> 
#> [[614]]
#> integer(0)
#> 
#> [[615]]
#> integer(0)
#> 
#> [[616]]
#> integer(0)
#> 
#> [[617]]
#> integer(0)
#> 
#> [[618]]
#> integer(0)
#> 
#> [[619]]
#> integer(0)
#> 
#> [[620]]
#> integer(0)
#> 
#> [[621]]
#> integer(0)
#> 
#> [[622]]
#> integer(0)
#> 
#> [[623]]
#> integer(0)
#> 
#> [[624]]
#> integer(0)
#> 
#> [[625]]
#> integer(0)
#> 
#> [[626]]
#> integer(0)
#> 
#> [[627]]
#> integer(0)
#> 
#> [[628]]
#> integer(0)
#> 
#> [[629]]
#> integer(0)
#> 
#> [[630]]
#> integer(0)
#> 
#> [[631]]
#> integer(0)
#> 
#> [[632]]
#> integer(0)
#> 
#> [[633]]
#> integer(0)
#> 
#> [[634]]
#> integer(0)
#> 
#> [[635]]
#> integer(0)
#> 
#> [[636]]
#> integer(0)
#> 
#> [[637]]
#> integer(0)
#> 
#> [[638]]
#> integer(0)
#> 
#> [[639]]
#> integer(0)
#> 
#> [[640]]
#> integer(0)
#> 
#> [[641]]
#> integer(0)
#> 
#> [[642]]
#> integer(0)
#> 
#> [[643]]
#> integer(0)
#> 
#> [[644]]
#> integer(0)
#> 
#> [[645]]
#> integer(0)
#> 
#> [[646]]
#> integer(0)
#> 
#> [[647]]
#> integer(0)
#> 
#> [[648]]
#> integer(0)
#> 
#> [[649]]
#> integer(0)
#> 
#> [[650]]
#> integer(0)
#> 
#> [[651]]
#> integer(0)
#> 
#> [[652]]
#> integer(0)
#> 
#> [[653]]
#> integer(0)
#> 
#> [[654]]
#> integer(0)
#> 
#> [[655]]
#> integer(0)
#> 
#> [[656]]
#> integer(0)
#> 
#> [[657]]
#> integer(0)
#> 
#> [[658]]
#> integer(0)
#> 
#> [[659]]
#> integer(0)
#> 
#> [[660]]
#> [1] 16 34  6
#> 
#> [[661]]
#> integer(0)
#> 
#> [[662]]
#> integer(0)
#> 
#> [[663]]
#> integer(0)
#> 
#> [[664]]
#> integer(0)
#> 
#> [[665]]
#> integer(0)
#> 
#> [[666]]
#> integer(0)
#> 
#> [[667]]
#> integer(0)
#> 
#> [[668]]
#> integer(0)
#> 
#> [[669]]
#> integer(0)
#> 
#> [[670]]
#> integer(0)
#> 
#> [[671]]
#> integer(0)
#> 
#> [[672]]
#> integer(0)
#> 
#> [[673]]
#> integer(0)
#> 
#> [[674]]
#> integer(0)
#> 
#> [[675]]
#> integer(0)
#> 
#> [[676]]
#> integer(0)
#> 
#> [[677]]
#> integer(0)
#> 
#> [[678]]
#> integer(0)
#> 
#> [[679]]
#> integer(0)
#> 
#> [[680]]
#> integer(0)
#> 
#> [[681]]
#> integer(0)
#> 
#> [[682]]
#> integer(0)
#> 
#> [[683]]
#> integer(0)
#> 
#> [[684]]
#> integer(0)
#> 
#> [[685]]
#> integer(0)
#> 
#> [[686]]
#> integer(0)
#> 
#> [[687]]
#> integer(0)
#> 
#> [[688]]
#> integer(0)
#> 
#> [[689]]
#> integer(0)
#> 
#> [[690]]
#> integer(0)
#> 
#> [[691]]
#> integer(0)
#> 
#> [[692]]
#> integer(0)
#> 
#> [[693]]
#> integer(0)
#> 
#> [[694]]
#> integer(0)
#> 
#> [[695]]
#> integer(0)
#> 
#> [[696]]
#> integer(0)
#> 
#> [[697]]
#> integer(0)
#> 
#> [[698]]
#> integer(0)
#> 
#> [[699]]
#> integer(0)
#> 
#> [[700]]
#> integer(0)
#> 
#> [[701]]
#> integer(0)
#> 
#> [[702]]
#> integer(0)
#> 
#> [[703]]
#> integer(0)
#> 
#> [[704]]
#> integer(0)
#> 
#> [[705]]
#> integer(0)
#> 
#> [[706]]
#> integer(0)
#> 
#> [[707]]
#> integer(0)
#> 
#> [[708]]
#> integer(0)
#> 
#> [[709]]
#> integer(0)
#> 
#> [[710]]
#> integer(0)
#> 
#> [[711]]
#> integer(0)
#> 
#> [[712]]
#> integer(0)
#> 
#> [[713]]
#> integer(0)
#> 
#> [[714]]
#> integer(0)
#> 
#> [[715]]
#> integer(0)
#> 
#> [[716]]
#> integer(0)
#> 
#> [[717]]
#> integer(0)
#> 
#> [[718]]
#> integer(0)
#> 
#> [[719]]
#> integer(0)
#> 
#> [[720]]
#> integer(0)
#> 
#> [[721]]
#> integer(0)
#> 
#> [[722]]
#> integer(0)
#> 
#> [[723]]
#> integer(0)
#> 
#> [[724]]
#> integer(0)
#> 
#> [[725]]
#> integer(0)
#> 
#> [[726]]
#> integer(0)
#> 
#> [[727]]
#> integer(0)
#> 
#> [[728]]
#> integer(0)
#> 
#> [[729]]
#> integer(0)
#> 
#> [[730]]
#> integer(0)
#> 
#> [[731]]
#> integer(0)
#> 
#> [[732]]
#> integer(0)
#> 
#> [[733]]
#> integer(0)
#> 
#> [[734]]
#> integer(0)
#> 
#> [[735]]
#> integer(0)
#> 
#> [[736]]
#> integer(0)
#> 
#> [[737]]
#> integer(0)
#> 
#> [[738]]
#> integer(0)
#> 
#> [[739]]
#> integer(0)
#> 
#> [[740]]
#> integer(0)
#> 
#> [[741]]
#> integer(0)
#> 
#> [[742]]
#> integer(0)
#> 
#> [[743]]
#> integer(0)
#> 
#> [[744]]
#> integer(0)
#> 
#> [[745]]
#> integer(0)
#> 
#> [[746]]
#> integer(0)
#> 
#> [[747]]
#> integer(0)
#> 
#> [[748]]
#> [1] 6
#> 
#> [[749]]
#> integer(0)
#> 
#> [[750]]
#> integer(0)
#> 
#> [[751]]
#> integer(0)
#> 
#> [[752]]
#> integer(0)
#> 
#> [[753]]
#> integer(0)
#> 
#> [[754]]
#> integer(0)
#> 
#> [[755]]
#> integer(0)
#> 
#> [[756]]
#> integer(0)
#> 
#> [[757]]
#> integer(0)
#> 
#> [[758]]
#> integer(0)
#> 
#> [[759]]
#> integer(0)
#> 
#> [[760]]
#> integer(0)
#> 
#> [[761]]
#> integer(0)
#> 
#> [[762]]
#> integer(0)
#> 
#> [[763]]
#> integer(0)
#> 
#> [[764]]
#> integer(0)
#> 
#> [[765]]
#> integer(0)
#> 
#> [[766]]
#> integer(0)
#> 
#> [[767]]
#> integer(0)
#> 
#> [[768]]
#> integer(0)
#> 
#> [[769]]
#> integer(0)
#> 
#> [[770]]
#> integer(0)
#> 
#> [[771]]
#> integer(0)
#> 
#> [[772]]
#> integer(0)
#> 
#> [[773]]
#> integer(0)
#> 
#> [[774]]
#> integer(0)
#> 
#> [[775]]
#> integer(0)
#> 
#> [[776]]
#> integer(0)
#> 
#> [[777]]
#> integer(0)
#> 
#> [[778]]
#> integer(0)
#> 
#> [[779]]
#> [1] 85
#> 
#> [[780]]
#> integer(0)
#> 
#> [[781]]
#> integer(0)
#> 
```
