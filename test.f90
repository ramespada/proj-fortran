program my_program

   use proj

   implicit none

   call test_proj()

contains

  subroutine test_proj()
     implicit none

     integer, parameter :: np = 4
     real(8), dimension(np) :: lats, lons
     integer :: i
     real(8) :: x, y
     character (len=*), parameter :: hirlam_rll_proj4 ="+proj=ob_tran +o_proj=longlat +o_lon_p=0 +o_lat_p=30 +lon_0=0"
     character (len=*), parameter ::      lonlat_proj4="EPSG:4326" !"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

     !! corners of EU meteo in projected lon-lat
     lons(1:np) = [ -26.,-26. , 40. , 40. ]
     lats(1:np) = [ -35., 22.5, 22.5,-35. ]


     print '("Original coordinates: ",4(F12.3))',lons,lats
     call proj_trans( lonlat_proj4   ,hirlam_rll_proj4, lons, lats)! forwards
     print '("After transformation: ", 4(F12.3))',lons,lats
     call proj_trans(hirlam_rll_proj4,lonlat_proj4    , lons, lats)! backwards
     print '("Reocvered coordinates:",4(F12.3))',lons,lats

!     !Aditional test: single value
!     print*,"(t) Aditional test 1: single value transformation.."
!     x=lons(1); y=lats(1)
!     print*,"original lons(1),y=lats(1) ",lons(1),lats(1)
!     call proj_trans(hirlam_rll_proj4,x,y)!,backwards)
!     print*,"transformed to x,y         ",x,y
!     call ll2proj_pt(hirlam_rll_proj4,x,y)!,forwards)
!     print*,"back to lons(1),y=lats(1): ",x,y
!     !call msg("Done")
!
!     !Aditional test: any 2 any
!     print*,"(t) Aditional test 2: any to any transformation.."
!     
  end subroutine test_proj

end program
