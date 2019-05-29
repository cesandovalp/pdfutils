library(pdftools)

GetTextAsRows = function( file_name )
{
  file_data.pdf = pdf_data( file_name )
  file_size.pdf = pdf_pagesize( file_name )
  file_info.pdf = pdf_info( file_name )

  all_pages_lines = vector()

  for( page_index in 1:file_info.pdf$pages )
  {
    page_data = file_data.pdf[[page_index]]
    page_size = file_size.pdf[page_index, ]

    frequency_x = as.data.frame( table( page_data$x ) )
    ordered     = order( frequency_x$Freq, decreasing = T )
    frequency   = frequency_x[ordered, ]

    x0 = vector()
    y0 = vector()
    x1 = vector()
    y1 = vector()

    # Store all boxes and draw them
    counter = 1
    for( row in 1:dim( page_data )[1] )
    {
      textbox = page_data[row, ]

      x0 = c( x0, textbox$x )
      y0 = c( y0, page_size$height - textbox$y )
      x1 = c( x1, x0[counter] + textbox$width )
      y1 = c( y1, y0[counter] - textbox$height )

      counter = counter + 1
    }

    # Remove columns names and header
    to_remove = which( y0 >= y1[which( x0 == min( x0 ) )] )
    to_remove = c( to_remove, which( y1 == min( y1 ) ) )
    x0 = x0[-to_remove]
    y0 = y0[-to_remove]
    x1 = x1[-to_remove]
    y1 = y1[-to_remove]
    ##########################################################################################################

    merged_x0 = vector()
    merged_y0 = vector()
    merged_x1 = vector()
    merged_y1 = vector()

    # Merge close boxes in x axe
    for( row in 1:length( x0 ) )
    {
      if( y0[row] %in% merged_y0 )
      {
        coincidences = which( merged_y0 == y0[row] )
        add_flag = TRUE
        for( index in coincidences )
        {
          if( ( merged_x1[index] < x0[row] ) && ( ( x0[row] - merged_x1[index] ) < ( page_size$width * .01 ) ) )
          {
            merged_x1[index] = x1[row];
            add_flag = FALSE
          }
        }
        if( add_flag )
        {
          merged_x0 = c( merged_x0, x0[row] )
          merged_y0 = c( merged_y0, y0[row] )
          merged_x1 = c( merged_x1, x1[row] )
          merged_y1 = c( merged_y1, y1[row] )
        }
      }
      else
      {
        merged_x0 = c( merged_x0, x0[row] )
        merged_y0 = c( merged_y0, y0[row] )
        merged_x1 = c( merged_x1, x1[row] )
        merged_y1 = c( merged_y1, y1[row] )
      }
    }

    index_rows = which( merged_x0 == min( merged_x0 ) )
    merged_x1[index_rows] = max( merged_x1 )

    ##########################################################################################################
    x0 = merged_x0
    y0 = merged_y0
    x1 = merged_x1
    y1 = merged_y1

    merged_x0 = vector()
    merged_y0 = vector()
    merged_x1 = vector()
    merged_y1 = vector()

    # Merge close boxes in y axe
    for( element_i in 1:length( x0 ) )
    {
      add_flag = TRUE

      if( length( merged_x0 ) > 0 )
      {
        for( element_j in 1:length( merged_x0 ) )
        {
          if( merged_x0[element_j] < x1[element_i] && merged_x1[element_j] > x0[element_i] &&
              merged_y1[element_j] < y0[element_i] && merged_y0[element_j] > y1[element_i] )
          {
            add_flag = FALSE

            if( merged_x0[element_j] > x0[element_i] )
              merged_x0[element_j] = x0[element_i]
            if( merged_x1[element_j] < x1[element_i] )
              merged_x1[element_j] = x1[element_i]
            if( merged_y0[element_j] < y0[element_i] )
              merged_y0[element_j] = y0[element_i]
            if( merged_y1[element_j] > y1[element_i] )
              merged_y1[element_j] = y1[element_i]
          }
        }
      }

      if( add_flag )
      {
        merged_x0 = c( merged_x0, x0[element_i] )
        merged_y0 = c( merged_y0, y0[element_i] )
        merged_x1 = c( merged_x1, x1[element_i] )
        merged_y1 = c( merged_y1, y1[element_i] )
      }
    }

    ##########################################################################################################
    dilatation_rate = 1

    while( nrow( as.data.frame( table( merged_x0 ) ) ) > 1 )
    {
      index_rows = which( merged_x0 == min( merged_x0 ) )

      if( length( merged_x0[-index_rows] ) == 0 )
        break

      merged_y0[-index_rows] = merged_y0[-index_rows] + dilatation_rate
      merged_y1[-index_rows] = merged_y1[-index_rows] - dilatation_rate

      x0 = merged_x0
      y0 = merged_y0
      x1 = merged_x1
      y1 = merged_y1

      merged_x0 = vector()
      merged_y0 = vector()
      merged_x1 = vector()
      merged_y1 = vector()

      # Merge close boxes in y axe
      for( element_i in 1:length( x0 ) )
      {
        add_flag = TRUE

        if( length( merged_x0 ) > 0 )
        {
          for( element_j in 1:length( merged_x0 ) )
          {
            if( merged_x0[element_j] < x1[element_i] && merged_x1[element_j] > x0[element_i] &&
                merged_y1[element_j] < y0[element_i] && merged_y0[element_j] > y1[element_i] )
            {
              add_flag = FALSE

              if( merged_y0[element_j] < ( y0[element_i] - dilatation_rate ) )
                merged_y0[element_j] = y0[element_i] - dilatation_rate
              if( merged_y1[element_j] > ( y1[element_i] + dilatation_rate ) )
                merged_y1[element_j] = y1[element_i] + dilatation_rate
            }
          }
        }

        if( add_flag )
        {
          merged_x0 = c( merged_x0, x0[element_i] )
          merged_y0 = c( merged_y0, y0[element_i] )
          merged_x1 = c( merged_x1, x1[element_i] )
          merged_y1 = c( merged_y1, y1[element_i] )
        }
      }

      dilatation_rate = dilatation_rate + 1
    }

    x0 = merged_x0
    y0 = merged_y0
    x1 = merged_x1
    y1 = merged_y1

    raw_text_line = vector( length = length( x0 ) )

    for( row in 1:length( x0 ) )
    {
      raw_text_line[row] = ""
      for( word in 1:dim( page_data )[1] )
      {
        textbox = page_data[word, ]
        word_x0 = textbox$x
        word_y0 = page_size$height - textbox$y
        word_x1 = word_x0 + textbox$width
        word_y1 = word_y0 - textbox$height

        if( word_x0 < x1[row] && word_x1 > x0[row] &&
            word_y1 < y0[row] && word_y0 > y1[row] )
        {
          raw_text_line[row] = paste( raw_text_line[row], textbox$text )
        }
      }
    }

    all_pages_lines = c( all_pages_lines, raw_text_line )
  }

  return( all_pages_lines )
}
