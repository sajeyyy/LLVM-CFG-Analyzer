define i32 @main() {
    %res = call i32 @SOURCE()
    call i32 @B(i32 %res)
    ret i32 %res
}

define i32 @B(i32 %arg) {
    call void @SINK(i32 %arg)
    ret i32 0
}

declare i32 @SOURCE()
declare void @SINK(i32)

