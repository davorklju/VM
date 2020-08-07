.text
    :main
        move.8  r0 len      //r0 = len
        load.8  r0 r0 0

        move.8  r1 mem_end  //r1 = data pointer
        load.16 r1 r1 0
        sub.16  r1 r0

        move.8  r2 0        //r2 = loop counter

        move.8  r3 1        //init data[len] = 1
        store.8 r1 r3 r0
    :main_loop
        cmp r2 r0
        jgt.16 main_loop_end

        push.8  r0
        push.16 r1
        push.8  r2

        call print_row

        load.8  r0 fp 0
        load.16 r1 fp 1

        call step

        pop.8 r2
        pop.16 r1
        pop.8 r0

        add.8 r2 1
        jmp.16 main_loop
    :main_loop_end
        halt
////////////////////////////////////////////////////////////
    :step               //r0 = len
                        //r1 = data pointer
        move.8 r2 0     //r2 = loop counter
        move.8 r3 1     //r3 = prev iteration left = data[current-1]
    :step_loop
        cmp.8 r2 r0
        jge.16 step_loop_end

        move.8 r4 r3
        ls.8 r4 1       //r4 = prev_left << 1

        load.8 r5 r1 r2 //r5 = data[current]
        or.8 r4 r5
        ls.8 r4 1       //r4 = (r4 | r5) << 1

        move.8 r3 r5    //r3 = prev left = data[current]

        move.8 r5 r2
        add.8 r5 1
        cmp.8 r5 r0
        jge.16 last_iter

        load.8 r5 r1 r5     //if not last iter
        or.8 r4 r5          //r4 = r4 | data[current+1]
        jmp.16 set_next_val
    :last_iter              //else
        or.8 r4 1           //r4 = r4 | 1
    :set_next_val
        move.8 r5 1

        cmp.8 r4 0      //if r4 == 0 || r4 == 4 || r4 == 7
        jeq.16 write_0     //then r5 = 0
        cmp.8 r4 4      //else r5 = 1
        jeq.16 write_0
        cmp.8 r4 7
        jeq.16 write_0
        jmp.16 write
    :write_0
        move.8 r5 0
    :write
        store.8 r1 r5 r2    //data[current] = r5
        add.8 r2 1          //inc loop counter
        jmp.16 step_loop
    :step_loop_end
        ret

////////////////////////////////////////////////////////////
    :print_row          //r1 = data pointer
        move.8 r2 0     //r2 = loop counter
        move.8 r3 r0    //r3 = len
    :print_row_loop
        cmp.8 r2 r3
        jgt.16 print_row_loop_end

        load.8 r0 r1 r2 //r0 = data[loop counter]   = *(r1 + r2) = r1[r2]
        cmp.8 r0 0
        jeq.16 blank
        move.8 r0 35    //r0 = '#'
        jmp.16 print_char
    :blank
        move.8 r0 32    //r0 = ' '
    :print_char
        trap print

        add.8 r2 1      //inc loop counter
        jmp.16 print_row_loop
    :print_row_loop_end
        move.8 r0 10    //r0 = '\n'
        trap print
        ret

.data
    :len       i8  125
    :mem_end   i16 499