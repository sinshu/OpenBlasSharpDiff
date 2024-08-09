using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPBSTF computes a split Cholesky factorization of a complex
        /// Hermitian positive definite band matrix A.
        /// </para>
        /// <para>
        /// This routine is designed to be used in conjunction with CHBGST.
        /// </para>
        /// <para>
        /// The factorization has the form  A = S**H*S  where S is a band matrix
        /// of the same bandwidth as A and the following structure:
        /// </para>
        /// <para>
        ///   S = ( U    )
        ///       ( M  L )
        /// </para>
        /// <para>
        /// where U is upper triangular of order m = (n+kd)/2, and L is lower
        /// triangular of order n-m.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="kb">
        /// No description available.
        /// </param>
        /// <param name="bb">
        /// No description available.
        /// </param>
        /// <param name="ldbb">
        /// No description available.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, the factorization could not be completed,
        /// because the updated element a(i,i) was negative; the
        /// matrix A is not positive definite.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The band storage scheme is illustrated by the following example, when
        ///  N = 7, KD = 2:
        /// </para>
        /// <para>
        ///  S = ( s11  s12  s13                     )
        ///      (      s22  s23  s24                )
        ///      (           s33  s34                )
        ///      (                s44                )
        ///      (           s53  s54  s55           )
        ///      (                s64  s65  s66      )
        ///      (                     s75  s76  s77 )
        /// </para>
        /// <para>
        ///  If UPLO = &#39;U&#39;, the array AB holds:
        /// </para>
        /// <para>
        ///  on entry:                          on exit:
        /// </para>
        /// <para>
        ///   *    *   a13  a24  a35  a46  a57   *    *   s13  s24  s53**H s64**H s75**H
        ///   *   a12  a23  a34  a45  a56  a67   *   s12  s23  s34  s54**H s65**H s76**H
        ///  a11  a22  a33  a44  a55  a66  a77  s11  s22  s33  s44  s55    s66    s77
        /// </para>
        /// <para>
        ///  If UPLO = &#39;L&#39;, the array AB holds:
        /// </para>
        /// <para>
        ///  on entry:                          on exit:
        /// </para>
        /// <para>
        ///  a11  a22  a33  a44  a55  a66  a77  s11    s22    s33    s44  s55  s66  s77
        ///  a21  a32  a43  a54  a65  a76   *   s12**H s23**H s34**H s54  s65  s76   *
        ///  a31  a42  a53  a64  a64   *    *   s13**H s24**H s53    s64  s75   *    *
        /// </para>
        /// <para>
        ///  Array elements marked * are not used by the routine; s12**H denotes
        ///  conjg(s12); the diagonal elements of S are real.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpbstf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpbstf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int kb,
            Complex32* bb,
            int ldbb);
    }
}
