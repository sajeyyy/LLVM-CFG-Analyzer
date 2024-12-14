define i32 @main() {
entry:
  %aVar = alloca i32
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %trueBranch, label %falseBranch

trueBranch:
  %secret = call i32 () @SOURCE()
  store i32 %secret, ptr %aVar
  br label %merge

falseBranch:
  store i32 0, ptr %aVar
  br label %merge

merge:
  %a = load i32, ptr %aVar
  %result = phi i32 [%a, %trueBranch], [0, %falseBranch]
  call void @SINK(i32 %result)
  ret i32 %result
}

declare i32 @SOURCE()
declare void @SINK(i32)

