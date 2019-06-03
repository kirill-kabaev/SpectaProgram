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
      integer start1, finish1, start2, finish2, rangex1, rangex2, mid1, mid2, h1, h2, i, nev, stat
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
      stat=0
      

         wsmi=wsmin/autocm
         wsma=wsmax/autocm
         emi=(emin+GZ)/autocm
         ema=(emax+GZ)/autocm
         smi=smin/autode**2
         
         open(unit=item, name='temporary.files.txt', status='new')
         open(unit=itemf, name='statestemporary.files.txt', status='old')
         open(unit=itemh, name='temporaryLevelE.files.txt', status='old')
         !open(unit=item,form='unformatted',recordtype='segmented')


         allocate(n(222811), x(222811), nmid2(276000000), nmid1(276000000))
         rewind itemf
         do i=1,222811     
			read(itemf, *) n(i), x(i)
			n(i) = int(n(i))
			x(i) = dble(x(i))
		 end do  
		 
		 
 		 do while(stat .EQ. 0)
		 read(itemh, *, iostat=stat) reee2, reee1, aa
		 i = i + 1
		
		 !write(item, 204) nev
   
         
        
    
			start1 =  1   
			finish1 = 222812
			start2 =  1   
			finish2 = 222812
			rangex1 = finish1 - start1
			rangex2 = finish2 - start2
			mid1 = start1 + (finish1-start1)/2
			mid2 = start2 + (finish2 - start2)/2   
			reee2=dble(reee2)
			reee1=dble(reee1)
         

				do while((x(mid1) .NE. reee2) .and. (rangex1 .GT. 0))
					h1=h1+1
					
					if (reee2 .GT. x(mid1)) then
					start1 = mid1 + 1
					else
						finish1 = mid1 - 1
					end if
					rangex1 = finish1 - start1
					mid1 = start1 + (finish1-start1)/2
					
				end do
			
				do while((x(mid2) .NE. reee1) .and. (rangex2 .GT. 0))
				h2=h2+1
					if (reee1 .GT. x(mid2)) then
					start2 = mid2 + 1
					else
						finish2 = mid2 - 1
					end if
					rangex2 = finish2 - start2
					mid2 = start2 + (finish2 - start2)/2
				
				end do

		
	
			write(item, 204) n(mid1), n(mid2), aa
			
		 end do
90       continue
         close(unit=itra)
         close(unit=itemf)
         close(unit=item)
         close(unit=itemh)
         deallocate(x, n, nmid2, nmid1)
         
!204   format(2f13.6, e15.7)
204   format(i8, i8, e15.7)
100   format(f13.6)
         
!204   format(i7, i7, e15.7)
!101   format(i6, f13.6)
!, i4, i2, 5i1,a2)
!204 format(i3, i3, i3, i3, i3, i3, i3, f9.6, f9.6, e10.3)



      end
