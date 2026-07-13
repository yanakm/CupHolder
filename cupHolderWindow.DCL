CupDialog : dialog {
    key = "CupDialog";
    label = "Cup Holder Generator";
        : column {
            : boxed_column {
                label = "Cup body";
                : row {
                    : spacer { width = 10; }
                    : edit_box {
                        key = "cup_height";
                        label = "Height [cm]:";
                        edit_width = 4;
                        fixed_width = true;
                    }
                }
                : row {
               		: spacer { width = 10; }
                    : edit_box {
                        key = "lower_dia";
                        label = "Lower Diameter [cm]:";
                        edit_width = 4;
                        fixed_width = true;
                    }
                    }
                : row {
                 	: spacer { width = 10; }
                    : edit_box {
                        key = "upper_dia";
                        label = "Upper Diameter [cm]:";
                        edit_width = 4;
                        fixed_width = true;
                    }
                }
                :row{
                	: spacer { width = 10; }
	                : toggle {
	                    key = "bottomless";
	                    label = "Bottomless";
	                    fixed_width = true;
	                }
                }
            }

            : boxed_column {
                label = "Cutout";
                : row {
                 : spacer { width = 10; }
                    : edit_box {
                        key = "cut_width";
                        label = "Width:";
                        edit_width = 4;
                        fixed_width = true;
                    }
                }
                :row {
                 : spacer { width = 10; }
                : edit_box {
                    key = "cut_top_dist";
                    label = "Cut Distance From Top:";
                    edit_width = 4;
                    fixed_width = true;
                }
                }
                :row {
                 : spacer { width = 10; }
                : edit_box {
                    key = "cut_bottom_dist";
                    label = "Distance From Bottom:";
                    edit_width = 4;
                    fixed_width = true;
                }
                }
            }
        
    : boxed_column {
        label = "Handle";

         : row {
         	: spacer { width = 10; }
            : edit_box {
                key = "handle_size";
                label = "Size [cm]:";
                edit_width = 4;
                fixed_width = true;
            }
        }
                 : row {
         	: spacer { width = 10; }
        : popup_list {
            key = "handle_type";
            label = "Type:";
            width = 18;
            fixed_width = true;
        }}

         : row {
         	: spacer { width = 10; }
        : popup_list {
            key = "handle_width";
            label = "Thickness:";
            width = 18;
            fixed_width = true;
        }}
    }
    }

    spacer;

    ok_cancel;
}