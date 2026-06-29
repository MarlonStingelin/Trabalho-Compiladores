.class public Programa
.super java/lang/Object

.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static maior(DD)D
	.limit stack 16
	.limit locals 16

	dload 0
	dload 1
	dcmpl
	ifgt L0
	goto L1
L0:
	dload 0
	d2i
	istore 2
	goto L2
L1:
	dload 1
	d2i
	istore 2
L2:
	iload 2
	i2d
	dreturn
	dconst_0
	dreturn
.end method

.method public static fat(I)I
	.limit stack 16
	.limit locals 16

	iconst_0
	istore 1
L3:
	iload 0
	iconst_0
	if_icmpgt L4
	goto L5
L4:
	iload 1
	iload 0
	imul
	istore 1
	iload 0
	iconst_1
	isub
	istore 0
	goto L3
L5:
	iload 1
	ireturn
	iconst_0
	ireturn
.end method

.method public static somatorio(I)I
	.limit stack 16
	.limit locals 16

	iconst_0
	i2d
	dstore 2
	iconst_0
	istore 1
L6:
	iload 1
	iload 0
	if_icmplt L7
	goto L8
L7:
	dload 2
	iload 1
	i2d
	dadd
	dstore 2
	iload 1
	iconst_1
	iadd
	istore 1
	goto L6
L8:
	dload 2
	d2i
	ireturn
	iconst_0
	ireturn
.end method

.method public static imprimir(Ljava/lang/String;D)V
	.limit stack 16
	.limit locals 16

	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	dload 1
	invokevirtual java/io/PrintStream/println(D)V
	return
	return
.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 16
	.limit locals 16

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Numero:"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	new java/util/Scanner
	dup
	getstatic java/lang/System/in Ljava/io/InputStream;
	invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
	invokevirtual java/util/Scanner/nextInt()I
	istore 1
	ldc2_w 4.5
	d2i
	invokestatic Programa/fat(I)I
	istore 0
	ldc2_w 2.5
	bipush 10
	i2d
	invokestatic Programa/maior(DD)D
	dstore 2
	ldc "teste:"
	iconst_1
	i2d
	invokestatic Programa/imprimir(Ljava/lang/String;D)V
	return
	return
.end method
