program select
    implicit none

    character(4) :: Title 
    character(1), parameter :: c = "z"
    integer :: DrMD = 0, PhD = 0
    integer :: MS = 0, BS = 0
    integer ::Others = 0 

    select case (Title) 
        case ("DrMD") 
            DrMD = DrMD + 1 
        case ("PhD") 
            PhD = PhD + 1 
        case ("MS") 
            MS = MS + 1 
        case ("BS") 
            BS = BS + 1 
        case default 
            Others = Others + 1 
    end select

    print *, Others


    select case (c) 
        case ('a' : 'j') 
            print *, 'First ten letters'
        case ('l' : 'p', 'u' : 'y') 
            print *, 'One of l,m,n,o,p,u,v,w,x,y' 
        case ('z', 'q' : 't') 
            print *, 'One of z,q,r,s,t' 
        case default
            print *, 'Other characters' 
    end select 

end program select