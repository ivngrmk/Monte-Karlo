program MonteKarlo
    !��, ��� ����� ��� ������ �������.
    use dflib
    type (wxycoord) wxy
    integer(2) logic
    !��������� �������.
    real, parameter :: av_l = 1.0 !������ ���������� �������.
    real, parameter :: pi = 3.14159265358979
    real t, distance
    real D !����������� ��������
    !��������� ���������
    integer, parameter :: Number0fStrikes = 1000000
    real, parameter :: dt = 0.1
    !��������.
    integer i, j
    !���������������.
    integer l_distr(100)
    real l, temp, angle
    !�������� ���� �������.
    type particle
        real x  !���������� .
        real y
        real z
        real vx !������������ ��������.
        real vy
        real vz
    end type particle
    type(particle) part
    !����� ��� ��������� �������� �����.
    call RANDOM_SEED()
    !��������� ������ �������.
    part.x = 0.0; part.y = 0.0; part.z = 0.0;
    call RANDOM_NUMBER(angle)
    angle = angle * pi * 2.0
    part.vx = cos(angle); part.vy = sin(angle); part.vz = 0.0;
    !��������� ��������� �������
    t = 0
    D = av_l * 1.0 / 3.0
    !�������� ������������ ���� � ���������� � �� ���� � ��������.
    call GraphicWindow()
    call GraphicAxes()
    !��������� ���� ��� ������ ����������� ������.
    open(1, file= "out.txt")
    !��� ��� �������� ������� �������� ����� �������, �� ����� ������� ���������� l �������� ����� ����� ����������.
    !��������� �������� ������� � ������ ����������  ��������.
    logic = SetColor(4)
    call MoveTo_w(DBLE(0.0), DBLE(0.0), wxy)
    do i = 1,Number0fStrikes
        part = ConvertPart( part, Get_Random_CosTetha(), Get_Random_Phi() )
        l = Get_Random_Distance()
        part.x = part.x + part.vx * l; part.y = part.y + part.vy * l; part.z = part.z + part.vz * l;
        t = t + l
        distance = sqrt( (part.x)**2 + (part.y)**2 + (part.z)**2 )
        logic= LineTo_w( DBLE(t), DBLE(distance) )
    end do
    
    !������������� ������.
    logic = SetColor(1)
    call MoveTo_w(DBLE(0.0), DBLE(0.0), wxy)
    temp = 0
    do while (temp < t)
        temp = temp + dt
        logic= LineTo_w( DBLE(temp), DBLE(sqrt(6*D*temp)) )
    end do
    
    close(1)
    
    contains
    
    real function Get_Random_Distance() !��������� ��������� ������ ������� �������.
        real r
        call RANDOM_NUMBER(r)
        Get_Random_Distance = -av_l * log( 1 - r )
    end function Get_Random_Distance
    
    real function Get_Random_CosTetha()    !��������� ���� � ��������� ���������� ������.
        real r
        call RANDOM_NUMBER(r)
        Get_Random_CosTetha = 1 - 2 * r
    end function Get_Random_CosTetha
    
    real function Get_Random_Phi()      !���������� ���� � ��������� ����������������� ����� ���������� ������.
        real r
        call RANDOM_NUMBER(r)
        Get_Random_Phi = 2 * pi * r
    end function Get_Random_Phi
    
    type(particle) function ConvertPart( old_part, cos_tetha,  phi ) !���������� ������ ������� �������� �� ����� ����������.
        type(particle) old_part
        real cos_tetha, phi
        ConvertPart.x = old_part.x; ConvertPart.y = old_part.y; ConvertPart.z = old_part.z;
        ConvertPart.vx = cos_tetha * old_part.vx - ( old_part.vy * sin(phi) - old_part.vx * old_part.vz * cos(phi) ) * sqrt ( (1 - (cos_tetha)**2) / (1 - (old_part.vz)**2) )
        ConvertPart.vy = cos_tetha * old_part.vy + ( old_part.vx * sin(phi) + old_part.vy * old_part.vz * cos(phi) ) * sqrt ( (1 - (cos_tetha)**2) / (1 - (old_part.vz)**2) )
        ConvertPart.vz = cos_tetha * old_part.vz - ( 1 - (old_part.vz)**2 ) * cos(phi) * sqrt ( (1 - (cos_tetha)**2) / (1 - (old_part.vz)**2) )
    end function ConvertPart
    
    subroutine GraphicWindow() !������ ���� ��� ������ �������. ��� - �����, ������� - ������, ���� ���� - �����.
        logical(2) bool2
        integer Pxl, Pxr, Pyu, Pyd
        Pxl = 100; Pyl = 50; Pxr = 900; Pyr = 650
        bool2 = SetBkColor(15); call ClearScreen(0) !������� ����� ������ � �����.
        bool2 = SetColor(0); bool2 = Rectangle($GBORDER,Pxl-1, Pyl-1, Pxr+1, Pyr+1) !�������� ������ ����.
        call SetViewPort(Pxl, Pyl, Pxr, Pyr) !�������� ������� �������.
        bool2 = SetBkColor(15); call ClearScreen(1) !������� ������� ������� ��� ������ � �����. 1 - ������ �� ��� ������� �������.
    end subroutine GraphicWindow
    
    subroutine GraphicAxes()
        real xl, yl, xr, yr, scale_width
        real x, y
        xl = -0.1; yl = -0.1; xr = 10000.0; yr = 300.0; scale_width = 0.1 !����������� ������ ��������� ������ ���������.
        bool2 = SetWindow(.TRUE., DBLE(xl), DBLE(yl), DBLE(xr), DBLE(yr))
        x = xl
        do while (ceiling(x) <= floor(xr)) !����������� ����� �������.
            call MoveTo_w(DBLE(ceiling(x)), DBLE(0.0 - scale_width), wxy)
            bool2 = LineTo_w(DBLE(ceiling(x)), DBLE(0.0 + scale_width))
            x = x + 1
        end do
        y = yl
        do while (ceiling(y) <= floor(yr)) !����������� ����� �������.
            call MoveTo_w(DBLE(0.0 - scale_width), DBLE(ceiling(y)), wxy)
            bool2 = LineTo_w(DBLE(0.0 + scale_width), DBLE(ceiling(y)))
            y = y + 1
        end do
        bool2 = SetColor(4) !������ ���� ���.
        call MoveTo_w(DBLE(xl), DBLE(0.0), wxy)
        bool2 = LineTo_w(DBLE(xr), DBLE(0.0))
        call MoveTo_w(DBLE(0.0), DBLE(yl), wxy)
        bool2 = LineTo_w(DBLE(0.0), DBLE(yr))
    end subroutine GraphicAxes
    
end