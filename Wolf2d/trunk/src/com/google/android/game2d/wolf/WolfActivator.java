package com.google.android.game2d.wolf;

import android.app.Activity;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.view.ViewGroup.LayoutParams;

import com.google.android.game2d.api.gui.BackgroundView;
import com.google.android.game2d.api.gui.SpriteView;
import com.google.android.game2d.wolf.sprites.FirstScene;
import com.google.android.game2d.wolf.sprites.wolverine.WolverineSprite;

public class WolfActivator extends Activity {
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle icicle) {
        super.onCreate(icicle);
       
        //ImageView background = new ImageView(this);
        //background.setBackgroundColor(Color.BLACK);
        
        Bitmap bitmap =  BitmapFactory.decodeResource(getResources(), R.drawable.wolverine);
        Bitmap scene = BitmapFactory.decodeResource(getResources(), R.drawable.tiles02);
        Bitmap bkg = BitmapFactory.decodeResource(getResources(), R.drawable.bkg01);
        
        //FirstScene background = new FirstScene(this, scene);
        BackgroundView background = new BackgroundView(this, bkg);
        
        WolverineSprite sprite = new WolverineSprite(bitmap, 0, 0, 0, 45);
        SpriteView v = new SpriteView(this, sprite);
        
        
        addContentView(background, new LayoutParams(LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT));
        
        LayoutParams parms = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);  
        
        addContentView(v, parms);
    }
}