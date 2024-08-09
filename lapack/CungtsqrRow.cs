using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CUNGTSQR_ROW generates an M-by-N complex matrix Q_out with
        /// orthonormal columns from the output of CLATSQR. These N orthonormal
        /// columns are the first N columns of a product of complex unitary
        /// matrices Q(k)_in of order M, which are returned by CLATSQR in
        /// a special format.
        /// </para>
        /// <para>
        ///      Q_out = first_N_columns_of( Q(1)_in * Q(2)_in * ... * Q(k)_in ).
        /// </para>
        /// <para>
        /// The input matrices Q(k)_in are stored in row and column blocks in A.
        /// See the documentation of CLATSQR for more details on the format of
        /// Q(k)_in, where each Q(k)_in is represented by block Householder
        /// transformations. This routine calls an auxiliary routine CLARFB_GETT,
        /// where the computation is performed on each individual block. The
        /// algorithm first sweeps NB-sized column blocks from the right to left
        /// starting in the bottom row block and continues to the top row block
        /// (hence _ROW in the routine name). This sweep is in reverse order of
        /// the order in which CLATSQR generates the output blocks.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A. M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="mb">
        /// [in] MB is INTEGER.
        /// The row block size used by CLATSQR to return
        /// arrays A and T. MB &gt; N.
        /// (Note that if MB &gt; M, then M is used instead of MB
        /// as the row block size).
        /// </param>
        /// <param name="nb">
        /// [in] NB is INTEGER.
        /// The column block size used by CLATSQR to return
        /// arrays A and T. NB &gt;= 1.
        /// (Note that if NB &gt; N, then N is used instead of NB
        /// as the column block size).
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// 
        /// On entry:
        /// 
        /// The elements on and above the diagonal are not used as
        /// input. The elements below the diagonal represent the unit
        /// lower-trapezoidal blocked matrix V computed by CLATSQR
        /// that defines the input matrices Q_in(k) (ones on the
        /// diagonal are not stored). See CLATSQR for more details.
        /// 
        /// On exit:
        /// 
        /// The array A contains an M-by-N orthonormal matrix Q_out,
        /// i.e the columns of A are orthogonal unit vectors.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [in] T is COMPLEX array,.
        /// dimension (LDT, N * NIRB)
        /// where NIRB = Number_of_input_row_blocks
        /// = MAX( 1, CEIL((M-N)/(MB-N)) )
        /// Let NICB = Number_of_input_col_blocks
        /// = CEIL(N/NB)
        /// 
        /// The upper-triangular block reflectors used to define the
        /// input matrices Q_in(k), k=(1:NIRB*NICB). The block
        /// reflectors are stored in compact form in NIRB block
        /// reflector sequences. Each of the NIRB block reflector
        /// sequences is stored in a larger NB-by-N column block of T
        /// and consists of NICB smaller NB-by-NB upper-triangular
        /// column blocks. See CLATSQR for more details on the format
        /// of T.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.
        /// LDT &gt;= max(1,min(NB,N)).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cungtsqr_row", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo CungtsqrRow(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int mb,
            int nb,
            Complex32* a,
            int lda,
            Complex32* t,
            int ldt);
    }
}
