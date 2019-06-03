program spect4

        implicit double precision(a-h,o-y), logical(z)
      character(len=8) title(9)
      namelist/prt/ zout, zsort, zspe, zpfun, itra, ilev, ispe, item, &
                    wsmax,wsmin, emin, emax, jmax, smin, gz, nf, ef, p1, p2, p3, p4, p5, p6, p7, p8, i, ne2, ne1
      common/logic/ zout, zsort, zspe, zpfun, zembed
      common/base/ ibase1,ibase2
      integer, parameter :: R = selected_real_kind(8, 6)
	  real(kind=R) :: reef, reee2, reee1
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: ss, a
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) ::  x, ee1, ee2
      
      integer, allocatable, dimension(:) :: n, nmid1, nmid2
      integer start1, finish1, start2, finish2, rangex1, rangex2, mid1, mid2, h1, h2, i
      data autocm/ 2.19474624d+05/, autode/ 2.5417662d0/, &
		   detosc/ 3.136186d-07/
      
     zout= .false.
      zsort= .true.
      zspe= .true.
      zpfun= .true.
      itra= 13
      ilev= 14
      ispe= 15
      item= 18
      itemf= 17
      itemh= 19
      wsmin= 0.0d0
      wsmax= 1.0d6
      smin=0.0d0
      jmax=500
      emin= -1.0d27
      emax= +1.0d27
      gz=0.0d0
      nr= 0
      

         wsmi=wsmin/autocm
         wsma=wsmax/autocm
         emi=(emin+GZ)/autocm
         ema=(emax+GZ)/autocm
         smi=smin/autode**2
         
         

    
		open(unit=itra,form='unformatted',recordtype='segmented')
         open(unit=item, name='temporary.files.txt', status='new')
         open(unit=itemh, name='temporaryLevelE.files.txt', status='new')
         !open(unit=item,form='unformatted',recordtype='segmented')
         rewind itra
         rewind itemh

		 
10       read(itra,end=90) j1,j2,kmin1,kmin2,neval1,neval2, &
                           idia,ipar,isym,gz,zembed,ibase1,ibase2
        ! print *, j1,j2,kmin1,kmin2,neval1,neval2, &
          !                 idia,ipar,isym,gz,zembed,ibase1,ibase2
         maxr=maxr+neval1*neval2
         if (max(j1,j2).gt.jmax) then
            do 9 ie2=1,neval2+2
            read(itra)
9           continue
            goto 10
         endif

         allocate(ee1(neval1), ee2(neval2), ss(neval1))
 
         
 ipar1=ipar
         if(neval1.gt.nmax1) nmax1= neval1
         if(neval2.gt.nmax2) nmax2= neval2
         read(itra) ee1
         read(itra) ee2
         if(idia.eq.-2.and.mod(j1,2).eq.1.and.ipar1.eq.0) then
               ipar1=mod(ipar1+j1,2)
         else if(idia.eq.-2.and.mod(j1,2).eq.1.and.ipar1.eq.1) then
               ipar1=0
         endif
         do 1 ie2=1,neval2
         read(itra) ss
         ee=ee2(ie2)
         if (ee.lt.emi .or. ee.gt.ema) goto 1
         do 2 ie1=1,neval1

         if (ee1(ie1).lt.emi) goto 2
         if (ee1(ie1).gt.ema) goto 1
         if (ss(ie1).lt.smi) goto 2
         w= ee - ee1(ie1)


        if (abs(w).ge.wsmi .and. abs(w).le.wsma) then
			if (w.ge.0.0d0) then 
            aa= w*w*w*autocm*autocm*autocm*ss(ie1)*autode*autode*detosc/dble(2*j2 + 1)
            write(itemh, 204) ee2(ie2)*autocm-gz, ee1(ie1)*autocm-gz, aa                               
		else 			
			aa= abs(w*w*w)*autocm*autocm*autocm*ss(ie1)*autode*autode*detosc/dble(2*j2 + 1)
			write(itemh, 204) ee1(ie1)*autocm-gz, ee2(ie2)*autocm-gz, aa  
  	                  
		end if      
       
        ! if (abs(w).ge.wsmi .and. abs(w).le.wsma) then     
        !     write(item, 204) (ee2(ie2)*autocm)-gz, (ee1(ie1)*autocm)-gz
              

           nr= nr + 1
         end if
  2      continue
  1      continue

         deallocate(ee1, ee2, ss)

         goto 10
90       continue
         close(unit=itra)
         close(unit=itemf)
         close(unit=item)
         close(unit=itemh)
         deallocate(x, n, nmid2, nmid1)

         
204   format(2f13.6, e15.7)
100   format(i12)

!204   format(i7, i7, e15.7)
!101   format(i6, f13.6)
!, i4, i2, 5i1,a2)
!204 format(i3, i3, i3, i3, i3, i3, i3, f9.6, f9.6, e10.3)



      end
