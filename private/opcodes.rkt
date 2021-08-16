#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [opcodes
   (vectorof (or/c
              (cons/c symbol? (listof (or/c 'u8 'u16 'u32 's8 's16 's32 0)))
              #f))]))

(define opcodes
  '#(;; Constants
     (nop) ; #x00
     (aconst_null)
     (iconst_m1)
     (iconst_0)
     (iconst_1)
     (iconst_2)
     (iconst_3)
     (iconst_4)
     (iconst_5)
     (lconst_0)
     (lconst_1)
     (fconst_0)
     (fconst_1)
     (fconst_2)
     (dconst_0)
     (dconst_1)
     (bipush s8) ; #x10
     (sipush s16)
     (ldc u8)
     (ldc_w u16)
     (ldc2_w u16)
     ;; Loads
     (iload u8)
     (lload u8)
     (fload u8)
     (dload u8)
     (aload u8)
     (iload_0)
     (iload_1)
     (iload_2)
     (iload_3)
     (lload_0)
     (lload_1)
     (lload_2) ; #x20
     (lload_3)
     (fload_0)
     (fload_1)
     (fload_2)
     (fload_3)
     (dload_0)
     (dload_1)
     (dload_2)
     (dload_3)
     (aload_0)
     (aload_1)
     (aload_2)
     (aload_3)
     (iaload)
     (laload)
     (faload) ; #x30
     (daload)
     (aaload)
     (baload)
     (caload)
     (saload)
     ;; Stores
     (istore u8)
     (lstore u8)
     (fstore u8)
     (dstore u8)
     (astore u8)
     (istore_0)
     (istore_1)
     (istore_2)
     (istore_3)
     (lstore_0)
     (lstore_1) ; #x40
     (lstore_2)
     (lstore_3)
     (fstore_0)
     (fstore_1)
     (fstore_2)
     (fstore_3)
     (dstore_0)
     (dstore_1)
     (dstore_2)
     (dstore_3)
     (astore_0)
     (astore_1)
     (astore_2)
     (astore_3)
     (iastore)
     (lastore) ; #x50
     (fastore)
     (dastore)
     (aastore)
     (bastore)
     (castore)
     (sastore)
     ;; Stack
     (pop)
     (pop2)
     (dup)
     (dup_x1)
     (dup_x2)
     (dup2)
     (dup2_x1)
     (dup2_x2)
     (swap)
     ;; Math
     (iadd) ; #x60
     (ladd)
     (fadd)
     (dadd)
     (isub)
     (lsub)
     (fsub)
     (dsub)
     (imul)
     (lmul)
     (fmul)
     (dmul)
     (idiv)
     (ldiv)
     (fdiv)
     (ddiv)
     (irem) ; #x70
     (lrem)
     (frem)
     (drem)
     (ineg)
     (lneg)
     (fneg)
     (dneg)
     (ishl)
     (lshl)
     (ishr)
     (lshr)
     (iushr)
     (lushr)
     (iand)
     (land)
     (ior) ; #x80
     (lor)
     (ixor)
     (lxor)
     (iinc u8 s8)
     ;; Conversions
     (i2l)
     (i2f)
     (i2d)
     (l2i)
     (l2f)
     (l2d)
     (f2i)
     (f2l)
     (f2d)
     (d2i)
     (d2l)
     (d2f) ; #x90
     (i2b)
     (i2c)
     (i2s)
     ;; Comparisons
     (lcmp)
     (fcmpl)
     (fcmpg)
     (dcmpl)
     (dcmpg)
     (ifeq s16)
     (ifne s16)
     (iflt s16)
     (ifge s16)
     (ifgt s16)
     (ifle s16)
     (if_icmpeq s16)
     (if_icmpne s16) ; #xA0
     (if_icmplt s16)
     (if_icmpge s16)
     (if_icmpgt s16)
     (if_icmple s16)
     (if_acmpeq s16)
     (if_acmpne s16)
     ;; Control
     (goto s16)
     (jsr s16)
     (ret u8)
     (tableswitch) ; special case
     (lookupswitch) ; special case
     (ireturn)
     (lreturn)
     (freturn)
     (dreturn)
     (areturn) ; #xB0
     (return)
     ;; References
     (getstatic u16)
     (putstatic u16)
     (getfield u16)
     (putfield u16)
     (invokevirtual u16)
     (invokespecial u16)
     (invokestatic u16)
     (invokeinterface u16 u8 0)
     (invokedynamic u16 0 0)
     (new u16)
     (newarray u8)
     (anewarray u16)
     (arraylength)
     (athrow)
     (checkcast u16) ; #xC0
     (instanceof u16)
     (monitorenter)
     (monitorexit)
     ;; Extended
     (wide) ; special case
     (multianewarray u16 u8)
     (ifnull s16)
     (ifnonnull s16)
     (goto_w s32)
     (jsr_w s32)
     (breakpoint)
     #f
     #f
     #f
     #f
     #f
     #f ; #xD0
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f ; #xE0
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f ; #xF0
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     #f
     (impdep1)
     (impdef2)))

(module+ test
  (require rackunit)
  (check-equal? (vector-length opcodes) 256))
