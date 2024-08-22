# proj-fortran
---

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![GitHub release](https://img.shields.io/github/release/CNEA-AQ/proj-fortran.svg)](https://github.com/CNEA-AQ/proj-fortran/releases/latest)
[![last-commit](https://img.shields.io/github/last-commit/CNEA-AQ/proj-fortran)](https://github.com/CNEA-AQ/proj-fortran/commits/main)


PROJ is a generic coordinate transformation software, that transforms coordinates from one coordinate reference system (CRS) to another. This includes cartographic projections as well as geodetic transformations.

This repo contains a C-Binding for Fortran of the C PROJ library.

### Dependences:
- [C-PROJ Library](https://proj.org/en/9.4/) (version > 6.0)


### Commands:
- [x] `call proj_trans( srs_in, srs_out, x, y) `

### How to use it:

Just call the library with the `use proj` statement on your Fortran code:

```fortran
program test_program
   use proj
   implicit none

   integer, parameter :: np = 4
   real(8), dimension(np) :: lats, lons
   character (len=*), parameter :: lcc_proj_example="+proj=lcc  +lon_0=-90 +lat_1=33 +lat_2=45"
   character (len=*), parameter ::     lonlat_proj4="EPSG:4326" !"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
  
   !! corners of EU meteo in projected lon-lat
   lons(1:np) = [ -26.,-26. , 40. , 40. ]
   lats(1:np) = [ -35., 22.5, 22.5,-35. ]
  
   print '("Original coordinates: ",4(F12.3))',lons,lats
   call proj_trans( lonlat_proj4   , lcc_proj_example, lons, lats)! forwards
   print '("After transformation: ", 4(F12.3))',lons,lats

end program
```

When compile, do not forget to add the `-lproj` flag, for example:

```shell
user@pc:~$ gfortran -c -O2 -ffree-line-length-none -w  -I/usr/include proj.f90 -o proj.o
user@pc:~$ gfortran -c -O2 -ffree-line-length-none -w  -I/usr/include test.f90 -o test.o
user@pc:~$ gfortran -O2 proj.o test.o -lproj  -o a.out
```

### To do list:

- [ ] Show how to compile PROJ from source
- [ ] If necesary look for another usefull commands.

