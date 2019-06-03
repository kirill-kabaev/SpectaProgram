program spect4

        implicit double precision(a-h,o-y), logical(z)
      character(len=8) title(9)
      namelist/prt/ zout, zsort, zspe, zpfun, itra, ilev, ispe, item, &
                    wsmax,wsmin, emin, emax, jmax, smin, gz, nf, ef, p1, p2, p3, p4, p5, p6, p7, p8, i, ne2, ne1
      common/logic/ zout, zsort, zspe, zpfun, zembed
      common/base/ ibase1,ibase2
      integer, parameter :: R = selected_real_kind(8, 6)
	  real(kind=R) :: reef, reee2, reee1
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) ::  b
      integer, allocatable, dimension(:,:) :: c
      integer, allocatable, dimension(:) :: a
      integer aval00, aval10, 	cval01, cval11, cval02, cval12, cval03, cval13, cval04, cval14, cval05, cval15, cval06, cval16, cval07, cval17, F
      real bval00, bval10
      character dval00
      !character(len=:),allocatable :: d(:)
      character(3), dimension(222811) :: d


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
      wsmin= 0.0d0
      wsmax= 1.0d6
      smin=0.0d0
      jmax=500
      emin= -1.0d27
      emax= +1.0d27
      gz=0.0d0
      

         wsmi=wsmin/autocm
         wsma=wsmax/autocm
         emi=(emin+GZ)/autocm
         ema=(emax+GZ)/autocm
         smi=smin/autode**2
         

         open(unit=itemf, name='states.17.PES1.txt', status='old')
         open(unit=item, name='statestemporary.files.txt', status='new')
         !open(unit=item,form='unformatted',recordtype='segmented')
         rewind itemf
 
      
         
         
			allocate(a(222811), b(222811), c(222811,7))
         do i=1,222811
			read(itemf, *) a(i), b(i), (c(i,ic),ic=1,7), d(i)
			a(i)=a(i)
			b(i)=b(i)
			c(i,1)=c(i,1)
			c(i,2)=c(i,2)
			c(i,3)=c(i,3)
			c(i,4)=c(i,4)
			c(i,5)=c(i,5)
			c(i,6)=c(i,6)
			c(i,7)=c(i,7)
			d(i)=d(i)
			
         enddo
        rewind item	
        rewind itemf	

        
                 
            do jr=222811,1,-1
            do ir=1,jr  
            if (b(ir) .gt. b(ir+1)) then 
				aval00=a(ir)
				a(ir)=a(ir+1)
				a(ir+1)=aval00
				
				
				
				bval00=b(ir)
				b(ir)=b(ir+1)
				b(ir+1)=bval00
				
				cval01=c(ir,1)
				c(ir,1)=c(ir+1,1)
				c(ir+1,1)=cval01
				
				cval02=c(ir,2)
				c(ir,2)=c(ir+1,2)
				c(ir+1,2)=cval02
				
				cval03=c(ir,3)
				c(ir,3)=c(ir+1,3)
				c(ir+1,3)=cval03
				
				cval04=c(ir,4)
				c(ir,4)=c(ir+1,4)
				c(ir+1,4)=cval04
				
				cval05=c(ir,5)
				c(ir,5)=c(ir+1,5)
				c(ir+1,5)=cval05
				
				cval06=c(ir,6)
				c(ir,6)=c(ir+1,6)
				c(ir+1,6)=cval06
		
				cval07=c(ir,7)
				c(ir,7)=c(ir+1,7)
				c(ir+1,7)=cval07
			
				dval00=d(ir)
				d(ir)=d(ir+1)
				d(ir+1)=dval00		
				endif
			end do
			end do
			
		do k=1,222811
			write(item, 204) a(k), b(k), (c(k,ic),ic=1,7), d(k)
        enddo
			

					


        deallocate(a, b, c)


         close(unit=itemf)
         close(unit=item)
204   format(i7, f15.7, i7, i4, 5i4, a4)

!101   format(i6, f13.6)
!, i4, i2, 5i1,a2)
!204 format(i3, i3, i3, i3, i3, i3, i3, f9.6, f9.6, e10.3)
      end
