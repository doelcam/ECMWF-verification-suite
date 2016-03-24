;-------------------------------------------------------------------------------;
; This function assigns the proper Giorgi domain for plotting in a map		;
; Need to give it a four letter code (G on the end for "Giorgi"), and it will	;
; give you back the four lat/lon values for that region				;
; Nothing special going on here, just keeps code tidy				;
;				Dave MacLeod, 26th April, University of Oxford	;
;-------------------------------------------------------------------------------;

undef("AssignGiorgiDomain")
function AssignGiorgiDomain(RegionNameCode)

begin

;-----------------------------------------------------------------------------------
; These are just here for reference
;AllRegionCodeList=(/"NHEX","SHEX","Trop","TROP","AUSG","AMZG","SSAG","CAMG","WNAG","CNAG","ENAG","ALAG","GRLG","MEDG","NEUG","WAFG","EAFG","SAFG","SAHG","SEAG","EASG","SASG","CASG","TIBG","NASG"/)

;AllRegionNameList=(/"NH extra tropics","SH extratropics","Tropics (20-20)","Tropics", "Australia (AUS)","Amazon Basin (AMZ)","Southern South America (SSA)","Central America (CAM)","Western North America (WNA)","Central North America (CNA)","Eastern North America (ENA)","Alaska (ALA)","Greenland (GRL)","Mediterranean Basin (MED)","Northern Europe (NEU)","Western Africa (WAF)","Eastern Africa (EAF)","Southern Africa (SAF)","Sahara (SAH)","Southeast Asia (SEA)", "East Asia (EAS)","South Asia (SAS)","Central Asia (CAS)","Tibet (TIB)","North Asia (NAS)"/)
;-----------------------------------------------------------------------------------

Domain=new(4,float)

if (RegionNameCode.eq."PERU")
    Domain(0)=-4.0
    Domain(1)=7.25
    Domain(2)=-81.5
    Domain(3)=79.0
else
if (RegionNameCode.eq."FERT")
    Domain(0)=41.5
    Domain(1)=30.5
    Domain(2)=32.5
    Domain(3)=50.5
else
if (RegionNameCode.eq."NHEX")
    Domain(0)=90.
    Domain(1)=20.
    Domain(2)=-180.
    Domain(3)=180.
else
if (RegionNameCode.eq."SHEX")
    Domain(0)=-20.
    Domain(1)=-90.
    Domain(2)=-180.
    Domain(3)=180.
else
if (RegionNameCode.eq."Trop")
    Domain(0)=20.
    Domain(1)=-20.
    Domain(2)=-180.
    Domain(3)=180.
else
if (RegionNameCode.eq."TROP")
    Domain(0)=30.
    Domain(1)=-30.
    Domain(2)=-180.
    Domain(3)=180.
else
if (RegionNameCode.eq."AUSG")
    Domain(0)=-11
    Domain(1)=-45.
    Domain(2)=110
    Domain(3)=155
else
if (RegionNameCode.eq."AMZG")
    Domain(0)=12
    Domain(1)=-20.
    Domain(2)=-82
    Domain(3)=-34.0
else
if (RegionNameCode.eq."SSAG")
    Domain(0)=-20
    Domain(1)=-56.
    Domain(2)=-76
    Domain(3)=-40
else
if (RegionNameCode.eq."CAMG")
    Domain(0)=30
    Domain(1)=10
    Domain(2)=-116
    Domain(3)=-83
else
if (RegionNameCode.eq."WNAG")
    Domain(0)=60
    Domain(1)=30
    Domain(2)=-130
    Domain(3)=-103
else
if (RegionNameCode.eq."CNAG")
    Domain(0)=50
    Domain(1)=30
    Domain(2)=-103
    Domain(3)=-85
else
if (RegionNameCode.eq."ENAG")
    Domain(0)=50
    Domain(1)=25
    Domain(2)=-85
    Domain(3)=-60
else
if (RegionNameCode.eq."ALAG")
    Domain(0)=72
    Domain(1)=60
    Domain(2)=-170
    Domain(3)=-103
else
if (RegionNameCode.eq."GRLG")
    Domain(0)=85
    Domain(1)=50
    Domain(2)=-103
    Domain(3)=-10
else
if (RegionNameCode.eq."MEDG")
    Domain(0)=48
    Domain(1)=30
    Domain(2)=-10
    Domain(3)=40
else
if (RegionNameCode.eq."NEUG")
    Domain(0)=75
    Domain(1)=48
    Domain(2)=-10
    Domain(3)=40
else
if (RegionNameCode.eq."WAFG")
    Domain(0)=18.0
    Domain(1)=-12.0
    Domain(2)=-20
    Domain(3)=22.0
else
if (RegionNameCode.eq."EAFG")
    Domain(0)=18.0
    Domain(1)=-12.0
    Domain(2)=22
    Domain(3)=52
else
if (RegionNameCode.eq."SAFG")
    Domain(0)=-12
    Domain(1)=-35
    Domain(2)=-10
    Domain(3)=52
else
if (RegionNameCode.eq."SAHG")
    Domain(0)=30
    Domain(1)=18
    Domain(2)=-20
    Domain(3)=65
else
if (RegionNameCode.eq."SEAG")
    Domain(0)=20
    Domain(1)=-11
    Domain(2)=95
    Domain(3)=155
else
if (RegionNameCode.eq."EASG")
    Domain(0)=50
    Domain(1)=20
    Domain(2)=100
    Domain(3)=145
else
if (RegionNameCode.eq."SASG")
    Domain(0)=30
    Domain(1)=5
    Domain(2)=65
    Domain(3)=100
else
if (RegionNameCode.eq."CASG")
    Domain(0)=50
    Domain(1)=30
    Domain(2)=40
    Domain(3)=75
else
if (RegionNameCode.eq."TIBG")
    Domain(0)=50
    Domain(1)=30
    Domain(2)=75
    Domain(3)=100
else
if (RegionNameCode.eq."NASG")
    Domain(0)=70
    Domain(1)=50
    Domain(2)=40
    Domain(3)=180
else
	print("I do not recognise the region code")
	print("Sorry, exiting")
	exit
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
end if
Domain@info = "Ordering is Latitude (Max), Latitude (Min) then Longitude (Min), Longitude (Max). With respect to a -90->90 & -180->180 coordinate system"

return(Domain)
end
