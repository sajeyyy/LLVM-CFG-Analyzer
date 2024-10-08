define i32 @main(i32 %argc) {
entry:
  %cmp = icmp eq i32 %argc, 1
  br i1 %cmp, label %then, label %else

then:
  %val1 = add i32 1, 0
  br label %end

else:
  %val2 = add i32 2, 0
  br label %end

end:
  %res = phi i32 [%val1, %then], [%val2, %else]
  ret i32 %res
}

