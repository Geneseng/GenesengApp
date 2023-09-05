// Upload TABULAR file
$(document).ready(function(){
             $('#upload_file').change(function() {
                var filename = $(this).val();
                if(filename!=""){
                    setTimeout(function(){
                        $('.upload-wrapper').addClass("uploaded");
                    }, 600);
                    setTimeout(function(){
                        $('.upload-wrapper').removeClass("uploaded");
                        $('.upload-wrapper').addClass("success");
                    }, 1600);
                }
            });
        });

// Upload IMAGING file
$(document).ready(function(){
             $('#filepicker').change(function() {
                var filename = $(this).val();
                if(filename!=""){
                    setTimeout(function(){
                        $('.upload-wrapper').addClass("uploaded");
                    }, 600);
                    setTimeout(function(){
                        $('.upload-wrapper').removeClass("uploaded");
                        $('.upload-wrapper').addClass("success");
                    }, 1600);
                }
            });
        });
