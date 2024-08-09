using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTFTTR copies a triangular matrix A from rectangular full packed
        /// format (TF) to standard full format (TR).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transr">
        /// [in] TRANSR is CHARACTER*1.
        /// = &#39;N&#39;:  ARF is in Normal format;
        /// = &#39;C&#39;:  ARF is in Conjugate-transpose format;
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular;
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="arf">
        /// [in] ARF is COMPLEX array, dimension ( N*(N+1)/2 ),.
        /// On entry, the upper or lower triangular matrix A stored in
        /// RFP format. For a further discussion see Notes below.
        /// </param>
        /// <param name="a">
        /// [out] A is COMPLEX array, dimension ( LDA, N ).
        /// On exit, the triangular matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of the array A contains
        /// the upper triangular matrix, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of the array A contains
        /// the lower triangular matrix, and the strictly upper
        /// triangular part of A is not referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  We first consider Standard Packed Format when N is even.
        ///  We give an example where N = 6.
        /// </para>
        /// <para>
        ///      AP is Upper             AP is Lower
        /// </para>
        /// <para>
        ///   00 01 02 03 04 05       00
        ///      11 12 13 14 15       10 11
        ///         22 23 24 25       20 21 22
        ///            33 34 35       30 31 32 33
        ///               44 45       40 41 42 43 44
        ///                  55       50 51 52 53 54 55
        /// </para>
        /// <para>
        ///  Let TRANSR = &#39;N&#39;. RFP holds AP as follows:
        ///  For UPLO = &#39;U&#39; the upper trapezoid A(0:5,0:2) consists of the last
        ///  three columns of AP upper. The lower triangle A(4:6,0:2) consists of
        ///  conjugate-transpose of the first three columns of AP upper.
        ///  For UPLO = &#39;L&#39; the lower trapezoid A(1:6,0:2) consists of the first
        ///  three columns of AP lower. The upper triangle A(0:2,0:2) consists of
        ///  conjugate-transpose of the last three columns of AP lower.
        ///  To denote conjugate we place -- above the element. This covers the
        ///  case N even and TRANSR = &#39;N&#39;.
        /// </para>
        /// <para>
        ///         RFP A                   RFP A
        /// </para>
        /// <para>
        ///                                -- -- --
        ///        03 04 05                33 43 53
        ///                                   -- --
        ///        13 14 15                00 44 54
        ///                                      --
        ///        23 24 25                10 11 55
        /// </para>
        /// <para>
        ///        33 34 35                20 21 22
        ///        --
        ///        00 44 45                30 31 32
        ///        -- --
        ///        01 11 55                40 41 42
        ///        -- -- --
        ///        02 12 22                50 51 52
        /// </para>
        /// <para>
        ///  Now let TRANSR = &#39;C&#39;. RFP A in both UPLO cases is just the conjugate-
        ///  transpose of RFP A above. One therefore gets:
        /// </para>
        /// <para>
        ///           RFP A                   RFP A
        /// </para>
        /// <para>
        ///     -- -- -- --                -- -- -- -- -- --
        ///     03 13 23 33 00 01 02    33 00 10 20 30 40 50
        ///     -- -- -- -- --                -- -- -- -- --
        ///     04 14 24 34 44 11 12    43 44 11 21 31 41 51
        ///     -- -- -- -- -- --                -- -- -- --
        ///     05 15 25 35 45 55 22    53 54 55 22 32 42 52
        /// </para>
        /// <para>
        ///  We next  consider Standard Packed Format when N is odd.
        ///  We give an example where N = 5.
        /// </para>
        /// <para>
        ///     AP is Upper                 AP is Lower
        /// </para>
        /// <para>
        ///   00 01 02 03 04              00
        ///      11 12 13 14              10 11
        ///         22 23 24              20 21 22
        ///            33 34              30 31 32 33
        ///               44              40 41 42 43 44
        /// </para>
        /// <para>
        ///  Let TRANSR = &#39;N&#39;. RFP holds AP as follows:
        ///  For UPLO = &#39;U&#39; the upper trapezoid A(0:4,0:2) consists of the last
        ///  three columns of AP upper. The lower triangle A(3:4,0:1) consists of
        ///  conjugate-transpose of the first two   columns of AP upper.
        ///  For UPLO = &#39;L&#39; the lower trapezoid A(0:4,0:2) consists of the first
        ///  three columns of AP lower. The upper triangle A(0:1,1:2) consists of
        ///  conjugate-transpose of the last two   columns of AP lower.
        ///  To denote conjugate we place -- above the element. This covers the
        ///  case N odd  and TRANSR = &#39;N&#39;.
        /// </para>
        /// <para>
        ///         RFP A                   RFP A
        /// </para>
        /// <para>
        ///                                   -- --
        ///        02 03 04                00 33 43
        ///                                      --
        ///        12 13 14                10 11 44
        /// </para>
        /// <para>
        ///        22 23 24                20 21 22
        ///        --
        ///        00 33 34                30 31 32
        ///        -- --
        ///        01 11 44                40 41 42
        /// </para>
        /// <para>
        ///  Now let TRANSR = &#39;C&#39;. RFP A in both UPLO cases is just the conjugate-
        ///  transpose of RFP A above. One therefore gets:
        /// </para>
        /// <para>
        ///           RFP A                   RFP A
        /// </para>
        /// <para>
        ///     -- -- --                   -- -- -- -- -- --
        ///     02 12 22 00 01             00 10 20 30 40 50
        ///     -- -- -- --                   -- -- -- -- --
        ///     03 13 23 33 11             33 11 21 31 41 51
        ///     -- -- -- -- --                   -- -- -- --
        ///     04 14 24 34 44             43 44 22 32 42 52
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctfttr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctfttr(
            MatrixLayout matrixLayout,
            char transr,
            char uplo,
            int n,
            Complex32* arf,
            Complex32* a,
            int lda);
    }
}
