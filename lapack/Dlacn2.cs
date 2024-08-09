using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DLACN2 estimates the 1-norm of a square, real matrix A.
        /// Reverse communication is used for evaluating matrix-vector products.
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 1.
        /// </param>
        /// <param name="v">
        /// [out] V is DOUBLE PRECISION array, dimension (N).
        /// On the final return, V = A*W,  where  EST = norm(V)/norm(W)
        /// (W is not returned).
        /// </param>
        /// <param name="x">
        /// [in,out] X is DOUBLE PRECISION array, dimension (N).
        /// On an intermediate return, X should be overwritten by
        /// A * X,   if KASE=1,
        /// A**T * X,  if KASE=2,
        /// and DLACN2 must be re-called with all the other parameters
        /// unchanged.
        /// </param>
        /// <param name="isgn">
        /// [out] ISGN is INTEGER array, dimension (N).
        /// </param>
        /// <param name="est">
        /// [in,out] EST is DOUBLE PRECISION.
        /// On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be
        /// unchanged from the previous call to DLACN2.
        /// On exit, EST is an estimate (a lower bound) for norm(A).
        /// </param>
        /// <param name="kase">
        /// [in,out] KASE is INTEGER.
        /// On the initial call to DLACN2, KASE should be 0.
        /// On an intermediate return, KASE will be 1 or 2, indicating
        /// whether X should be overwritten by A * X  or A**T * X.
        /// On the final return from DLACN2, KASE will again be 0.
        /// </param>
        /// <param name="isave">
        /// [in,out] ISAVE is INTEGER array, dimension (3).
        /// ISAVE is used to save variables between calls to DLACN2
        /// </param>
        /// <remarks>
        /// <para>
        ///  Originally named SONEST, dated March 16, 1988.
        /// </para>
        /// <para>
        ///  This is a thread safe version of DLACON, which uses the array ISAVE
        ///  in place of a SAVE statement, as follows:
        /// </para>
        /// <para>
        ///     DLACON     DLACN2
        ///      JUMP     ISAVE(1)
        ///      J        ISAVE(2)
        ///      ITER     ISAVE(3)
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dlacn2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dlacn2(
            int n,
            double* v,
            double* x,
            int* isgn,
            double* est,
            int* kase,
            int* isave);
    }
}
