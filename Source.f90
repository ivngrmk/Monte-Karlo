program MonteKarlo
    !То, что нужно для работы графики.
    use dflib
    type (wxycoord) wxy
    integer(2) logic
    !Параметры системы.
    real, parameter :: av_l = 1.0 !Длинна свободного пробега.
    real, parameter :: pi = 3.14159265358979
    real t, distance
    real D !Коэффициент диффуции
    !Параметры симуляции
    integer, parameter :: Number0fStrikes = 1000000
    real, parameter :: dt = 0.1
    !Счётчики.
    integer i, j
    !Вспомогательные.
    integer l_distr(100)
    real l, temp, angle
    !Создание типа ЧАСТИЦА.
    type particle
        real x  !Координаты .
        real y
        real z
        real vx !Направляющие косинусы.
        real vy
        real vz
    end type particle
    type(particle) part
    !Нужно для получения случаных чисел.
    call RANDOM_SEED()
    !Начальные данные частицы.
    part.x = 0.0; part.y = 0.0; part.z = 0.0;
    call RANDOM_NUMBER(angle)
    angle = angle * pi * 2.0
    part.vx = cos(angle); part.vy = sin(angle); part.vz = 0.0;
    !Начальные параметры системы
    t = 0
    D = av_l * 1.0 / 3.0
    !Создание графического окна и прорисовка в нём окна с графиком.
    call GraphicWindow()
    call GraphicAxes()
    !Открываем файл для записи необходимых данных.
    open(1, file= "out.txt")
    !Так как скорость частицы численно равно единице, то время пробега расстояния l численно равно этому расстоянию.
    !Симуляция движуния частицы в случае трёхмерного  движения.
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
    
    !Теоретическая прямая.
    logic = SetColor(1)
    call MoveTo_w(DBLE(0.0), DBLE(0.0), wxy)
    temp = 0
    do while (temp < t)
        temp = temp + dt
        logic= LineTo_w( DBLE(temp), DBLE(sqrt(6*D*temp)) )
    end do
    
    close(1)
    
    contains
    
    real function Get_Random_Distance() !Получание случайной длинны пробега частицы.
        real r
        call RANDOM_NUMBER(r)
        Get_Random_Distance = -av_l * log( 1 - r )
    end function Get_Random_Distance
    
    real function Get_Random_CosTetha()    !Получение угла в плоскости соударения частиц.
        real r
        call RANDOM_NUMBER(r)
        Get_Random_CosTetha = 1 - 2 * r
    end function Get_Random_CosTetha
    
    real function Get_Random_Phi()      !Получаение угла в плоскости перпенддикулярной линии соударения частиц.
        real r
        call RANDOM_NUMBER(r)
        Get_Random_Phi = 2 * pi * r
    end function Get_Random_Phi
    
    type(particle) function ConvertPart( old_part, cos_tetha,  phi ) !Построение нового вектора скорости по углам соударения.
        type(particle) old_part
        real cos_tetha, phi
        ConvertPart.x = old_part.x; ConvertPart.y = old_part.y; ConvertPart.z = old_part.z;
        ConvertPart.vx = cos_tetha * old_part.vx - ( old_part.vy * sin(phi) - old_part.vx * old_part.vz * cos(phi) ) * sqrt ( (1 - (cos_tetha)**2) / (1 - (old_part.vz)**2) )
        ConvertPart.vy = cos_tetha * old_part.vy + ( old_part.vx * sin(phi) + old_part.vy * old_part.vz * cos(phi) ) * sqrt ( (1 - (cos_tetha)**2) / (1 - (old_part.vz)**2) )
        ConvertPart.vz = cos_tetha * old_part.vz - ( 1 - (old_part.vz)**2 ) * cos(phi) * sqrt ( (1 - (cos_tetha)**2) / (1 - (old_part.vz)**2) )
    end function ConvertPart
    
    subroutine GraphicWindow() !Создаёт окно для вывода графики. фон - белый, граница - чёрная, само окно - белое.
        logical(2) bool2
        integer Pxl, Pxr, Pyu, Pyd
        Pxl = 100; Pyl = 50; Pxr = 900; Pyr = 650
        bool2 = SetBkColor(15); call ClearScreen(0) !Окраска всего экрана в белый.
        bool2 = SetColor(0); bool2 = Rectangle($GBORDER,Pxl-1, Pyl-1, Pxr+1, Pyr+1) !Создание границ окна.
        call SetViewPort(Pxl, Pyl, Pxr, Pyr) !Создание рабочей области.
        bool2 = SetBkColor(15); call ClearScreen(1) !Окраска рабочей области под график в белый. 1 - ссылка на эту рабочую область.
    end subroutine GraphicWindow
    
    subroutine GraphicAxes()
        real xl, yl, xr, yr, scale_width
        real x, y
        xl = -0.1; yl = -0.1; xr = 10000.0; yr = 300.0; scale_width = 0.1 !Обязательно должны содержать начало координат.
        bool2 = SetWindow(.TRUE., DBLE(xl), DBLE(yl), DBLE(xr), DBLE(yr))
        x = xl
        do while (ceiling(x) <= floor(xr)) !Градуировка шкалы абсцисс.
            call MoveTo_w(DBLE(ceiling(x)), DBLE(0.0 - scale_width), wxy)
            bool2 = LineTo_w(DBLE(ceiling(x)), DBLE(0.0 + scale_width))
            x = x + 1
        end do
        y = yl
        do while (ceiling(y) <= floor(yr)) !Градуировка шкалы ординат.
            call MoveTo_w(DBLE(0.0 - scale_width), DBLE(ceiling(y)), wxy)
            bool2 = LineTo_w(DBLE(0.0 + scale_width), DBLE(ceiling(y)))
            y = y + 1
        end do
        bool2 = SetColor(4) !Рисуем сами оси.
        call MoveTo_w(DBLE(xl), DBLE(0.0), wxy)
        bool2 = LineTo_w(DBLE(xr), DBLE(0.0))
        call MoveTo_w(DBLE(0.0), DBLE(yl), wxy)
        bool2 = LineTo_w(DBLE(0.0), DBLE(yr))
    end subroutine GraphicAxes
    
end