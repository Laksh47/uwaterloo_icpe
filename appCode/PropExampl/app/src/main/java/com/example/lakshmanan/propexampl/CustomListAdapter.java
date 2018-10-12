package com.example.lakshmanan.propexampl;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.bumptech.glide.Glide;

/**
 * Created by lakshmanan on 16/02/18.
 */

public class CustomListAdapter extends ArrayAdapter<String> {
    private final Activity context;
    private final String[] prices;
    private final String[] imgurls;
    private final String[] titles;

    public CustomListAdapter(Activity context, String[] prices, String[] imgurls, String[] titles) {
        super(context, R.layout.mylist, prices);
        // TODO Auto-generated constructor stub

        this.context=context;
        this.prices=prices;
        this.imgurls=imgurls;
        this.titles=titles;
    }

    public View getView(int position,View view,ViewGroup parent) {
        LayoutInflater inflater=context.getLayoutInflater();
        View rowView=inflater.inflate(R.layout.mylist, null,true);

        TextView txtTitle = (TextView) rowView.findViewById(R.id.item);
        ImageView imageView = (ImageView) rowView.findViewById(R.id.icon);
        TextView extratxt = (TextView) rowView.findViewById(R.id.textView1);

        txtTitle.setText(prices[position]);
        Glide.with(this.context).load(imgurls[position]).into(imageView);
        extratxt.setText(titles[position]);
        return rowView;
    }
}
