using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZLACN2 estimates the 1-norm of a square, complex matrix A.
        /// Reverse communication is used for evaluating matrix-vector products.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 1.
        /// </param>
        /// <param name="v">
        /// [out] V is COMPLEX*16 array, dimension (N).
        /// On the final return, V = A*W,  where  EST = norm(V)/norm(W)
        /// (W is not returned).
        /// </param>
        /// <param name="x">
        /// [in,out] X is COMPLEX*16 array, dimension (N).
        /// On an intermediate return, X should be overwritten by
        /// A * X,   if KASE=1,
        /// A**H * X,  if KASE=2,
        /// where A**H is the conjugate transpose of A, and ZLACN2 must be
        /// re-called with all the other parameters unchanged.
        /// </param>
        /// <param name="est">
        /// [in,out] EST is DOUBLE PRECISION.
        /// On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be
        /// unchanged from the previous call to ZLACN2.
        /// On exit, EST is an estimate (a lower bound) for norm(A).
        /// </param>
        /// <param name="kase">
        /// [in,out] KASE is INTEGER.
        /// On the initial call to ZLACN2, KASE should be 0.
        /// On an intermediate return, KASE will be 1 or 2, indicating
        /// whether X should be overwritten by A * X  or A**H * X.
        /// On the final return from ZLACN2, KASE will again be 0.
        /// </param>
        /// <param name="isave">
        /// [in,out] ISAVE is INTEGER array, dimension (3).
        /// ISAVE is used to save variables between calls to ZLACN2
        /// </param>
        /// <remarks>
        /// <para>
        ///  Originally named CONEST, dated March 16, 1988.
        /// </para>
        /// <para>
        ///  Last modified:  April, 1999
        /// </para>
        /// <para>
        ///  This is a thread safe version of ZLACON, which uses the array ISAVE
        ///  in place of a SAVE statement, as follows:
        /// </para>
        /// <para>
        ///     ZLACON     ZLACN2
        ///      JUMP     ISAVE(1)
        ///      J        ISAVE(2)
        ///      ITER     ISAVE(3)
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zlacn2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zlacn2(
            int n,
            Complex* v,
            Complex* x,
            double* est,
            int* kase,
            int* isave);
    }
}
